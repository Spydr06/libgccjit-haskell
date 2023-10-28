{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import qualified GccJit

import Foreign.Ptr
import Foreign.C (CString, newCString)
import System.Exit
import System.IO

foreign import ccall "dynamic"
    mkFun :: FunPtr (CString -> ()) -> (CString -> ())

unwrapOrDie :: IO (Maybe a) -> String -> IO a
unwrapOrDie x msg = do
    x' <- x
    case x' of
        Nothing -> die msg
        Just x'' -> return x''

createCode :: Ptr GccJit.Context -> IO ()
createCode ctxt = 
    {-
        Let's try to inject the equivalent of:
        void greet (const char *name)
        {
            printf ("hello %s\n", name);
        }
    -} 
    return ()

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

    -- Extract the generated code from "result".
    greet <- unwrapOrDie (GccJit.resultGetCode result "greet") "NULL greet"
    
    -- Now call the generated function:
    mkFun greet <$> newCString "world"
    hFlush stdout

    GccJit.contextRelease ctxt
    GccJit.resultRelease result


