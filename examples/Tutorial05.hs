module Main (main) where

{- Ported from https://gcc.gnu.org/onlinedocs/jit/intro/tutorial05.html -}

import System.Environment
import System.IO

import qualified GccJit
import qualified GccJit.Types

import System.Exit (exitFailure, die, exitSuccess)
import Foreign.Ptr
import Control.Monad (when)
import Data.Char (ord)
import Data.Foldable (foldlM)

unwrapOrDie :: IO (Maybe a) -> String -> IO a
unwrapOrDie x msg = do
    x' <- x
    case x' of
        Nothing -> die msg
        Just x'' -> return x''

makeMain :: Ptr GccJit.Context -> IO (Ptr GccJit.Function)
makeMain ctxt = do
    intType <- GccJit.contextGetType ctxt GccJit.Int
    argcParam <- GccJit.contextNewParam ctxt nullPtr intType "argc"
    charType <- GccJit.contextGetType ctxt GccJit.Char
    charPtrType <- GccJit.typeGetPointer charType
    charPtrPtrType <- GccJit.typeGetPointer charPtrType
    argvParam <- GccJit.contextNewParam ctxt nullPtr charPtrPtrType "argv"
    GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionExported intType "main" [argcParam, argvParam] False

data BfCompiler = BfCompiler {
    filename :: String,
    line :: Int,
    column :: Int,

    ctxt :: Ptr GccJit.Context,

    voidType :: Ptr GccJit.Type,
    intType :: Ptr GccJit.Type,
    byteType :: Ptr GccJit.Type,
    arrayType :: Ptr GccJit.Type,

    funcGetchar :: Ptr GccJit.Function,
    funcPutchar :: Ptr GccJit.Function,

    func :: Ptr GccJit.Function,
    curblock :: Ptr GccJit.Block,

    intZero :: Ptr GccJit.RValue,
    intOne :: Ptr GccJit.RValue,
    byteZero :: Ptr GccJit.RValue,
    byteOne :: Ptr GccJit.RValue,
    dataCells :: Ptr GccJit.LValue,
    idx :: Ptr GccJit.LValue,

    parenTest :: [Ptr GccJit.Block],
    parenBody :: [Ptr GccJit.Block],
    parenAfter :: [Ptr GccJit.Block]
}

-- Bail out, with a message on stderr
fatalError :: BfCompiler -> String -> IO a
fatalError bfc msg = do
    hPutStrLn stderr $ filename bfc ++ ":" ++ show (line bfc) ++ ":" ++ show (line bfc) ++ ": " ++ msg
    exitFailure

-- Get "data_cells[idx] as an lvalue."
bfGetCurrentData :: BfCompiler -> Ptr GccJit.Location -> IO (Ptr GccJit.LValue)
bfGetCurrentData bfc loc = do
    dataCells <- GccJit.lValueAsRValue $ dataCells bfc
    idx <- GccJit.lValueAsRValue $ idx bfc
    GccJit.contextNewArrayAccess (ctxt bfc) loc dataCells idx

-- Get "data_cells[idx] == 0" as a boolean rvalue.
bfCurrentDataIsZero :: BfCompiler -> Ptr GccJit.Location -> IO (Ptr GccJit.RValue)
bfCurrentDataIsZero bfc loc = do
    cell <- bfGetCurrentData bfc loc >>= GccJit.lValueAsRValue
    GccJit.contextNewComparison (ctxt bfc) loc GccJit.ComparisonEq cell (byteZero bfc)

traceEnabled = False

-- Trace execution by injectiong putchar () of each source char.
bfTrace :: BfCompiler -> Ptr GccJit.Location -> Char -> IO ()
bfTrace bfc loc ch = when traceEnabled $ do
    arg <- GccJit.contextNewRValueFromInt (ctxt bfc) (intType bfc) $ ord ch
    call <- GccJit.contextNewCall (ctxt bfc) loc (funcPutchar bfc) [arg]
    GccJit.blockAddEval (curblock bfc) loc call

-- Compile one bf character.
bfCompileChar :: BfCompiler -> Char -> IO BfCompiler
bfCompileChar bfc ch = do
    loc <- GccJit.contextNewLocation (ctxt bfc) (filename bfc) (line bfc) $ column bfc
    bfTrace bfc loc ch
    let bfc' = bfc { column = succ $ column bfc }
    case ch of
        '>' -> do
            GccJit.blockAddComment (curblock bfc) loc "'>': idx += 1;"
            GccJit.blockAddAssignmentOp (curblock bfc) loc (idx bfc) GccJit.BinaryPlus $ intOne bfc
            return bfc'
        '<' -> do
            GccJit.blockAddComment (curblock bfc) loc "'<': idx -= 1;"
            GccJit.blockAddAssignmentOp (curblock bfc) loc (idx bfc) GccJit.BinaryMinus $ intOne bfc
            return bfc'
        '+' -> do
            GccJit.blockAddComment (curblock bfc) loc "'+': data[idx] += 1;"
            cell <- bfGetCurrentData bfc loc
            GccJit.blockAddAssignmentOp (curblock bfc) loc cell GccJit.BinaryPlus $ byteOne bfc
            return bfc'
        '-' -> do
            GccJit.blockAddComment (curblock bfc) loc "'-': data[idx] -= 1;"
            cell <- bfGetCurrentData bfc loc
            GccJit.blockAddAssignmentOp (curblock bfc) loc cell GccJit.BinaryMinus $ byteOne bfc
            return bfc'
        '.' -> do
            cell <- bfGetCurrentData bfc loc >>= GccJit.lValueAsRValue
            arg <- GccJit.contextNewCast (ctxt bfc) loc cell $ intType bfc
            call <- GccJit.contextNewCall (ctxt bfc) loc (funcPutchar bfc) [arg]
            GccJit.blockAddComment (curblock bfc) loc "'.': putchar ((int)data[idx]);"
            GccJit.blockAddEval (curblock bfc) loc call
            return bfc'
        ',' -> do
            call <- GccJit.contextNewCall (ctxt bfc) loc (funcGetchar bfc) []
            GccJit.blockAddComment (curblock bfc) loc "',': data[idx] = (unsigned char)getchar ();"
            cell <- bfGetCurrentData bfc loc
            GccJit.contextNewCast (ctxt bfc) loc call (byteType bfc) >>= GccJit.blockAddAssignment (curblock bfc) loc cell 
            return bfc'
        '[' -> do
            loopTest <- unwrapOrDie (GccJit.functionNewBlock (func bfc) Nothing) "NULL loop_test"
            onZero <- unwrapOrDie (GccJit.functionNewBlock (func bfc) Nothing) "NULL on_zero"
            onNonZero <- unwrapOrDie (GccJit.functionNewBlock (func bfc) Nothing) "NULL on_non_zero"

            GccJit.blockEndWithJump (curblock bfc) loc loopTest
            GccJit.blockAddComment loopTest loc "'['"
            cond <- bfCurrentDataIsZero bfc loc
            GccJit.blockEndWithConditional loopTest loc cond onZero onNonZero
            return bfc' {
                parenTest = parenTest bfc ++ [loopTest],
                parenBody = parenBody bfc ++ [onNonZero],
                parenAfter = parenAfter bfc ++ [onZero],
                curblock = onNonZero
            }
        ']' -> do
            GccJit.blockAddComment (curblock bfc) loc "']'"

            when (null $ parenTest bfc) $ do fatalError bfc "mismatching parens"

            GccJit.blockEndWithJump (curblock bfc) loc $ last $ parenTest bfc
            return bfc' {
                curblock = last $ parenAfter bfc,
                parenTest = init $ parenTest bfc,
                parenBody = init $ parenBody bfc,
                parenAfter = init $ parenAfter bfc
            }
        '\n' -> return bfc {
                line = succ $ line bfc,
                column = 0
            }
        _ -> return bfc { column = succ $ column bfc }

-- Compile the given .bf file into a gcc_jit_context, containing a
-- single "main" function suitable for compiling into an executable.

bfCompile :: String -> IO (Ptr GccJit.Context)
bfCompile filename = do
    ctxt <- unwrapOrDie GccJit.contextAcquire "NULL context"
    GccJit.setIntOption ctxt GccJit.OptimizationLevel 3
    GccJit.setBoolOption ctxt GccJit.DumpInitialGimple False
    GccJit.setBoolOption ctxt GccJit.DebugInfo True
    GccJit.setBoolOption ctxt GccJit.DumpEverything False
    GccJit.setBoolOption ctxt GccJit.KeepIntermediate False

    voidType <- GccJit.contextGetType ctxt GccJit.Void
    intType <- GccJit.contextGetType ctxt GccJit.Int
    byteType <- GccJit.contextGetType ctxt GccJit.UnsignedChar
    arrayType <- GccJit.contextNewArrayType ctxt nullPtr byteType 30000

    funcGetchar <- GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionImported intType "getchar" [] False
    paramC <- GccJit.contextNewParam ctxt nullPtr intType "c"
    funcPutchar <- GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionImported voidType "putchar" [paramC] False

    func <- makeMain ctxt
    initialBlock <- unwrapOrDie (GccJit.functionNewBlock func $ Just "initial") "NULL initial"
    intZero <- GccJit.contextZero ctxt intType
    intOne <- GccJit.contextOne ctxt intType
    byteZero <- GccJit.contextZero ctxt byteType
    byteOne <- GccJit.contextOne ctxt byteType

    dataCells <- GccJit.contextNewGlobal ctxt nullPtr GccJit.GlobalInternal arrayType "data_cells"
    idx <- GccJit.functionNewLocal func nullPtr intType "idx"

    GccJit.blockAddComment initialBlock nullPtr "idx = 0;"
    GccJit.blockAddAssignment initialBlock nullPtr idx intZero

    let bfc = BfCompiler filename 1 0 ctxt voidType intType byteType arrayType funcGetchar funcPutchar func initialBlock intZero intOne byteZero byteOne dataCells idx [] [] [] in 
        do
            bfc' <- readFile filename >>= foldlM bfCompileChar bfc
            GccJit.blockEndWithReturn (curblock bfc') nullPtr intZero
            return ctxt

-- Entrypoint to the compiler.

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            ctxt <- bfCompile inputFile
            GccJit.contextCompileToFile ctxt GccJit.Executable outputFile
            err <- GccJit.contextGetFirstError ctxt
            GccJit.contextRelease ctxt
            case err of
                Just _ -> exitFailure
                Nothing -> exitSuccess
        _ -> do
            prog <- getProgName
            hPutStrLn stderr $ prog ++ ": INPUT_FILE OUTPUT_FILE"
            exitFailure

