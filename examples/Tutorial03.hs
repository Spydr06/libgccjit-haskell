{-# LANGUAGE ForeignFunctionInterface #-}

{- Ported from https://gcc.gnu.org/onlinedocs/jit/intro/tutorial03.html -}

module Main (main) where

import qualified GccJit
import GccJit.Utils (release, asRValue)

import Foreign.Ptr
import Foreign.C (CString, newCString)
import System.Exit
import System.IO

foreign import ccall "dynamic" mkFun :: FunPtr (Int -> IO Int) -> (Int -> IO Int)

unwrapOrDie :: IO (Maybe a) -> String -> IO a
unwrapOrDie x msg = do
    x' <- x
    case x' of
        Nothing -> die msg
        Just x'' -> return x''

createCode :: Ptr GccJit.Context -> IO ()
createCode ctxt = do
    {-
        Simple sum-of-squares, to test conditionals and looping

        int loop_test (int n)
        {
          int i;
          int sum = 0;
          for (i = 0; i < n ; i ++)
          {
	    sum += i * i;
          }
          return sum;
    -}
    theType <- GccJit.contextGetType ctxt GccJit.Int
    n <- GccJit.contextNewParam ctxt nullPtr theType "n"
    func <- GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionExported theType "loop_test" [n] False

    -- Build locals:
    i <- GccJit.functionNewLocal func nullPtr theType "i"
    sum <- GccJit.functionNewLocal func nullPtr theType "sum"

    bInitial <- unwrapOrDie (GccJit.functionNewBlock func $ Just "initial") "NULL initial"
    bLoopCond <- unwrapOrDie (GccJit.functionNewBlock func $ Just "loop_cond") "NULL loop_cond"
    bLoopBody <- unwrapOrDie (GccJit.functionNewBlock func $ Just "loop_body") "NULL loop_body"
    bAfterLoop <- unwrapOrDie (GccJit.functionNewBlock func $ Just "after_loop") "NULL after_loop"

    -- sum = 0;
    zero <- GccJit.contextZero ctxt theType
    GccJit.blockAddAssignment bInitial nullPtr sum zero

    -- i = 0;
    GccJit.blockAddAssignment bInitial nullPtr i zero

    GccJit.blockEndWithJump bInitial nullPtr bLoopCond

    -- if (i >= n)
    i' <- asRValue i
    n' <- asRValue n
    comparison <- GccJit.contextNewComparison ctxt nullPtr GccJit.ComparisonGe i' n'
    GccJit.blockEndWithConditional bLoopCond nullPtr comparison bAfterLoop bLoopBody

    -- sum += i * i
    mult <- GccJit.contextNewBinaryOp ctxt nullPtr GccJit.BinaryMult theType i' i'
    GccJit.blockAddAssignmentOp bLoopBody nullPtr sum GccJit.BinaryPlus mult

    -- i++
    one <- GccJit.contextOne ctxt theType
    GccJit.blockAddAssignmentOp bLoopBody nullPtr i GccJit.BinaryPlus one

    GccJit.blockEndWithJump bLoopBody nullPtr bLoopCond

    -- return sum
    sum' <- asRValue sum
    GccJit.blockEndWithReturn bAfterLoop nullPtr sum'
    
main :: IO ()
main = do
    -- Get a "context" object for working with the library.
    ctxt <- unwrapOrDie GccJit.contextAcquire "NULL ctxt"

    -- Set some options on the context.
    -- Let's see the code being generated, in assembler form.
    GccJit.setBoolOption ctxt GccJit.DumpGeneratedCode True

    -- Populate the context.
    createCode ctxt

    -- Compile the code.
    result <- unwrapOrDie (GccJit.contextCompile ctxt) "NULL result"

    -- We're done with the context; we can release it:
    release ctxt

    fnPtr <- unwrapOrDie (GccJit.resultGetCode result "loop_test") "NULL fnPtr"
    val <- mkFun fnPtr 10
    putStrLn $ "loop_test returned: " ++ show val
    
    release result
