{-# LANGUAGE ForeignFunctionInterface #-}

{- Ported from https://gcc.gnu.org/onlinedocs/jit/intro/tutorial04.html -}

module Main (main) where

import qualified GccJit
import GccJit.Utils (release)

import Foreign.Ptr
import Foreign.C (CString, newCString)
import System.Exit
import System.IO
import System.FilePath
import Data.List
import Control.Monad (filterM)
import Data.Maybe (catMaybes, fromMaybe)
import System.Environment (getArgs, getProgName)
import Data.Ord (comparing)

data OpCode = 
    -- Ops taking no operand.
    Dup
    | Rot
    | BinaryAdd
    | BinarySubtract
    | BinaryMult
    | BinaryCompareLT
    | Recurse
    | Return
    -- Ops taking an operand.
    | PushConst Int
    | JumpAbsIfTrue Int
    deriving (Show, Eq)

data ToyVMOp = Op {
    opOpCode :: OpCode,
    opLinenum :: Int
} deriving (Eq)

data ToyVMFunction = ToyVMFunction {
    fnFilename :: String,
    fnOps :: [ToyVMOp]
}

data ToyVMFrame = ToyVMFrame {
    frmFunction :: ToyVMFunction,
    frmPc :: Int,
    frmStack :: [Int]
}

getFunctionName :: String -> String
getFunctionName filename = takeFileName filename \\ "."

parseLine :: String -> (Int, String) -> IO (Maybe ToyVMOp)
parseLine _ (_, '#':_) = return Nothing
parseLine _ (_, "") = return Nothing
parseLine _ (linenum, "DUP") = return $ Just $ Op Dup linenum 
parseLine _ (linenum, "ROT") = return $ Just $ Op Rot linenum
parseLine _ (linenum, "BINARY_ADD") = return $ Just $ Op BinaryAdd linenum
parseLine _ (linenum, "BINARY_SUBTRACT")= return $ Just $ Op BinarySubtract linenum
parseLine _ (linenum, "BINARY_MULT") = return $ Just $ Op BinaryMult linenum
parseLine _ (linenum, "BINARY_COMPARE_LT") = return $ Just $ Op BinaryCompareLT linenum
parseLine _ (linenum, "RECURSE") = return $ Just $ Op Recurse linenum
parseLine _ (linenum, "RETURN") = return $ Just $ Op Return linenum
parseLine _ (linenum, 'P':'U':'S':'H':'_':'C':'O':'N':'S':'T':xs) = return $ Just $ Op (PushConst $ read xs) linenum
parseLine _ (linenum, 'J':'U':'M':'P':'_':'A':'B':'S':'_':'I':'F':'_':'T':'R':'U':'E':xs) = return $ Just $ Op (JumpAbsIfTrue $ read xs) linenum
parseLine filename (linenum, line) = die $ filename ++ ":" ++ show linenum ++ ": parse error"

functionParse :: String -> String -> IO ToyVMFunction
functionParse filename name = do
    buf <- readFile filename
    ToyVMFunction filename . catMaybes <$> mapM (parseLine filename) (zip [1..] $ lines buf)

functionDisassembleOp :: ToyVMFunction -> Int -> ToyVMOp -> String
functionDisassembleOp fn index op = fnFilename fn ++ ":" ++ show (opLinenum op) ++ ": index " ++ show index ++ ": " ++ show (opOpCode op) ++ "\n"

functionDisassemble :: ToyVMFunction -> String
functionDisassemble fn = concat $ zipWith (functionDisassembleOp fn) [0..] $ fnOps fn

framePush :: ToyVMFrame -> Int -> ToyVMFrame
framePush frame arg = frame { frmStack = frmStack frame ++ [arg] }

framePop :: ToyVMFrame -> (Int, ToyVMFrame)
framePop frame = let stack = frmStack frame in
    (last stack, frame { frmStack = init stack})

frameDumpStack :: ToyVMFrame -> String
frameDumpStack frame = "stack: " ++ unwords (map show (frmStack frame))

opInterpret :: ToyVMFunction -> ToyVMFrame -> ToyVMOp -> Maybe Handle -> IO Int
opInterpret fn frame op trace = do
    case trace of
        Just out -> do
            hPutStrLn out $ frameDumpStack frame
            hPutStr out $ functionDisassembleOp fn (frmPc frame) op
        Nothing -> return ()
    newFrame <- case opOpCode op of
        Dup -> let (x, frame') = framePop frame in
            return $ framePush (framePush frame' x) x
        Rot -> let (y, frame') = framePop frame in
            let (x, frame'') = framePop frame' in
            return $ framePush (framePush frame'' y) x
        BinaryAdd -> let (y, frame') = framePop frame in
            let (x, frame'') = framePop frame' in
            return $ framePush frame'' (x + y)
        BinarySubtract -> let (y, frame') = framePop frame in
            let (x, frame'') = framePop frame' in
            return $ framePush frame'' (x - y)
        BinaryMult -> let (y, frame') = framePop frame in
            let (x, frame'') = framePop frame' in
            return $ framePush frame'' (x * y)
        BinaryCompareLT -> let (y, frame') = framePop frame in
            let (x, frame'') = framePop frame' in
            return $ framePush frame'' (fromEnum $ x < y)
        Recurse -> let (x, frame') = framePop frame in do
            x' <- functionInterpret fn x trace
            return $ framePush frame' x'
        Return -> return frame
        PushConst operand -> return $ framePush frame operand
        JumpAbsIfTrue operand -> let (x, frame') = framePop frame in
            return $ if x == 0 then frame' else frame' { frmPc = operand }
    case opOpCode op of
        Return -> return $ fst $ framePop newFrame
        _ -> let op' = fnOps fn !! frmPc newFrame in
            opInterpret fn newFrame { frmPc = succ $ frmPc newFrame } op' trace

functionInterpret :: ToyVMFunction -> Int -> Maybe Handle -> IO Int
functionInterpret fn arg trace = let frame = ToyVMFrame fn 0 [arg] in
    let op = fnOps fn !! frmPc frame in
        opInterpret fn frame { frmPc = succ $ frmPc frame } op trace
    
data CompilationState = CompilationState {
    ctxt :: Ptr GccJit.Context,
    
    intType :: Ptr GccJit.Type,
    boolType :: Ptr GccJit.Type,
    stackType :: Ptr GccJit.Type,

    constOne :: Ptr GccJit.RValue,

    curFn :: Ptr GccJit.Function,
    paramArg :: Ptr GccJit.Param,
    stack :: Ptr GccJit.LValue,
    stackDepth :: Ptr GccJit.LValue,
    xReg :: Ptr GccJit.LValue,
    yReg :: Ptr GccJit.LValue,

    opLocs :: [Ptr GccJit.Location],
    initialBlock :: Ptr GccJit.Block,
    opBlocks :: [Ptr GccJit.Block]
}

addPush :: CompilationState -> Ptr GccJit.Block -> Ptr GccJit.RValue -> Ptr GccJit.Location -> IO ()
addPush state block rvalue loc = do
    -- stack[stack_depth] = RVALUE
    rStack <- GccJit.asRValue $ stack state
    rStackDepth <- GccJit.asRValue $ stackDepth state
    -- stack[stack_depth]
    arrAcc <- GccJit.contextNewArrayAccess (ctxt state) loc rStack rStackDepth
    GccJit.blockAddAssignment block loc arrAcc rvalue
    -- "stack_depth++;".
    GccJit.blockAddAssignmentOp block loc (stackDepth state) GccJit.BinaryPlus $ constOne state

addPop :: CompilationState -> Ptr GccJit.Block -> Ptr GccJit.LValue -> Ptr GccJit.Location -> IO ()
addPop state block lvalue loc = do
    -- "-- stack_depth;".
    GccJit.blockAddAssignmentOp block loc (stackDepth state) GccJit.BinaryMinus $ constOne state
    -- "LVALUE = stack[stack_depth];".
    rStack <- GccJit.asRValue $ stack state
    rStackDepth <- GccJit.asRValue $ stackDepth state
    -- stack[stack_depth]
    arrAcc <- GccJit.contextNewArrayAccess (ctxt state) loc rStack rStackDepth
    rArrAcc <- GccJit.asRValue arrAcc
    GccJit.blockAddAssignment block loc lvalue rArrAcc

type ToyVMCompiledCode = Int -> IO Int

data ToyVMCompiledFunction = ToyVMCompiledFunction {
    cfJitResult :: Ptr GccJit.Result,
    cfCode :: ToyVMCompiledCode
}

maxStackDepth :: Int
maxStackDepth = 8

functionCompile :: ToyVMFunction -> IO ToyVMCompiledFunction
functionCompile fn = let funcname = getFunctionName $ fnFilename fn in do
    ctxt <- unwrapOrDie GccJit.contextAcquire "NULL context"
    GccJit.setBoolOption ctxt GccJit.DumpInitialGimple False
    GccJit.setBoolOption ctxt GccJit.DumpGeneratedCode False
    GccJit.setIntOption ctxt GccJit.OptimizationLevel 3
    GccJit.setBoolOption ctxt GccJit.KeepIntermediate False
    GccJit.setBoolOption ctxt GccJit.DumpEverything False
    GccJit.setBoolOption ctxt GccJit.DebugInfo True

    -- Create types.
    intType <- GccJit.contextGetType ctxt GccJit.Int
    boolType <- GccJit.contextGetType ctxt GccJit.Bool
    stackType <- GccJit.contextNewArrayType ctxt nullPtr intType maxStackDepth
    
    -- The constant value 1.
    constOne <- GccJit.contextOne ctxt intType

    -- Create locations.
    opLocs <- mapM (\op -> GccJit.contextNewLocation ctxt (fnFilename fn) (opLinenum op) 0 {- column -}) $ fnOps fn

    -- Creating the function.
    paramArg <- GccJit.contextNewParam ctxt (head opLocs) intType "arg"
    curFn <- GccJit.contextNewFunction ctxt (head opLocs) GccJit.FunctionExported intType funcname [paramArg] False

    -- Create stack lvalues.
    stack <- GccJit.functionNewLocal curFn nullPtr stackType "stack"
    stackDepth <- GccJit.functionNewLocal curFn nullPtr intType "stack_depth"
    xReg <- GccJit.functionNewLocal curFn nullPtr intType "x"
    yReg <- GccJit.functionNewLocal curFn nullPtr intType "y"

    -- 1st pass: create blocks, one per opcode.

    -- We need an entry block to do one-time initialization, so create that first
    initialBlock <- unwrapOrDie (GccJit.functionNewBlock curFn $ Just "initial") "NULL initial"

    -- Create a block per operation.
    opBlocks <- mapM (\(i, op) -> unwrapOrDie (GccJit.functionNewBlock curFn $ Just $ "instr" ++ show i) "NULL block") $ zip [0..] $ fnOps fn

    -- Populate the initial block.
    
    -- "stack_depth = 0;".
    zero <- GccJit.contextZero ctxt intType
    GccJit.blockAddAssignment initialBlock (head opLocs) stackDepth zero

    let state = CompilationState ctxt intType boolType stackType constOne curFn paramArg stack stackDepth xReg yReg opLocs initialBlock opBlocks in do
        -- "PUSH (arg);"
        rParam <- GccJit.paramAsRValue paramArg
        addPush state initialBlock rParam $ head opLocs
        
        -- ...and jump to insn 0.
        GccJit.blockEndWithJump initialBlock (head opLocs) $ head opBlocks
        
        -- 2nd pass: fill in instructions.
        state' <- generateOp state 0 (fnOps fn)

        result <- unwrapOrDie (GccJit.contextCompile ctxt) "NULL result"
        GccJit.release ctxt 

        -- We've now finished populating the context. Compile it.
        ToyVMCompiledFunction result . mkFun <$> unwrapOrDie (GccJit.resultGetCode result funcname) ("NULL " ++ funcname)
    where generateOp :: CompilationState -> Int -> [ToyVMOp] -> IO CompilationState
          generateOp state _ [] = do return state
          generateOp state pc (op:nextOps) = let loc = opLocs state !! pc in
            let block = opBlocks state !! pc in
            let nextBlock = if null nextOps then Nothing else Just $ opBlocks state !! (pc + 1) in
            let xEqualsPop = addPop state block (xReg state) loc in
            let yEqualsPop = addPop state block (yReg state) loc in
            let pushRValue rv = addPush state block rv loc in
            let pushX = do
                rX <- GccJit.asRValue $ xReg state
                pushRValue rX
            in
            let pushY = do
                rY <- GccJit.asRValue $ yReg state
                pushRValue rY
            in
            let next = generateOp state (succ pc) nextOps in
            let jmpNext = do
                GccJit.blockEndWithJump block loc $ fromMaybe nullPtr nextBlock
                next
            in
            case opOpCode op of
                Dup -> do
                    xEqualsPop
                    pushX
                    pushX
                    jmpNext
                Rot -> do
                    yEqualsPop
                    xEqualsPop
                    pushY
                    pushX
                    jmpNext
                BinaryAdd -> do
                    yEqualsPop
                    xEqualsPop
                    rX <- GccJit.asRValue $ xReg state
                    rY <- GccJit.asRValue $ yReg state
                    binOp <- GccJit.contextNewBinaryOp (ctxt state) loc GccJit.BinaryPlus (intType state) rX rY
                    pushRValue binOp
                    jmpNext
                BinarySubtract -> do
                    yEqualsPop
                    xEqualsPop
                    rX <- GccJit.asRValue $ xReg state
                    rY <- GccJit.asRValue $ yReg state
                    binOp <- GccJit.contextNewBinaryOp (ctxt state) loc GccJit.BinaryMinus (intType state) rX rY
                    pushRValue binOp
                    jmpNext
                BinaryMult -> do
                    yEqualsPop
                    xEqualsPop
                    rX <- GccJit.asRValue $ xReg state
                    rY <- GccJit.asRValue $ yReg state
                    binOp <- GccJit.contextNewBinaryOp (ctxt state) loc GccJit.BinaryMult (intType state) rX rY
                    pushRValue binOp
                    jmpNext
                BinaryCompareLT -> do
                    yEqualsPop
                    xEqualsPop
                    rX <- GccJit.asRValue $ xReg state
                    rY <- GccJit.asRValue $ yReg state
                    comparison <- GccJit.contextNewComparison (ctxt state) loc GccJit.ComparisonLt rX rY
                    cast <- GccJit.contextNewCast (ctxt state) loc comparison $ intType state
                    pushRValue cast
                    jmpNext
                Recurse -> do
                    xEqualsPop
                    arg <- GccJit.asRValue $ xReg state
                    call <- GccJit.contextNewCall (ctxt state) loc (curFn state) [arg]
                    pushRValue call
                    jmpNext
                Return -> do
                    xEqualsPop
                    rX <- GccJit.asRValue $ xReg state
                    GccJit.blockEndWithReturn block loc rX
                    next
                -- Ops taking an operand.
                PushConst operand -> do
                    rv <- GccJit.contextNewRValueFromInt (ctxt state) (intType state) operand
                    pushRValue rv
                    jmpNext
                JumpAbsIfTrue operand -> do
                    xEqualsPop
                    rX <- GccJit.asRValue $ xReg state
                    cast <- GccJit.contextNewCast (ctxt state) loc rX $ boolType state
                    GccJit.blockEndWithConditional block loc cast (opBlocks state !! operand) $ fromMaybe nullPtr nextBlock
                    next 

foreign import ccall "dynamic" mkFun :: FunPtr ToyVMCompiledCode -> ToyVMCompiledCode

unwrapOrDie :: IO (Maybe a) -> String -> IO a
unwrapOrDie x msg = do
    x' <- x
    case x' of
        Nothing -> die msg
        Just x'' -> return x''

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 
        then do
            prog <- getProgName 
            die $ show prog ++ " FILENAME INPUT: Parse and run a .toy file\n"
        else let filename = head args in do
            fn <- functionParse filename filename
            putStrLn $ functionDisassemble fn
            iResult <- functionInterpret fn (read $ args !! 1) $ Just stdout
            putStrLn $ "interpreter result: " ++ show iResult
            
            -- JIT-compilation.
            compiledFn <- functionCompile fn
            jitResult <- cfCode compiledFn $ read $ args !! 1
            putStrLn $ "compiler result: " ++ show jitResult

