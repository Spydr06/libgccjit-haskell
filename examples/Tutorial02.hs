{-# LANGUAGE ForeignFunctionInterface #-}

{- Ported from https://gcc.gnu.org/onlinedocs/jit/intro/tutorial02.html -}

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
        Let's try to inject the equivalent of:

          int square (int i)
          {
            return i * i;
          }
    -}
    intType <- GccJit.contextGetType ctxt GccJit.Int
    paramI <- GccJit.contextNewParam ctxt nullPtr intType "i"
    func <- GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionExported intType "square" [paramI] False

    block <- unwrapOrDie (GccJit.functionNewBlock func Nothing) "NULL block"
    i <- asRValue paramI
    expr <- GccJit.contextNewBinaryOp ctxt nullPtr GccJit.BinaryMult intType i i

    GccJit.blockEndWithReturn block nullPtr expr

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

    fnPtr <- unwrapOrDie (GccJit.resultGetCode result "square") "NULL fnPtr"
    squared <- mkFun fnPtr 5
    print squared
    
    release result
