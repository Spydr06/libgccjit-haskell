{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import qualified GccJit

import Foreign.Ptr
import Foreign.C (CString, newCString)
import System.Exit
import System.IO

foreign import ccall "dynamic"
    mkFun :: FunPtr (CString -> ()) -> (CString -> ())

dieIfNull :: Ptr a -> String -> IO ()
dieIfNull ptr msg = if ptr == nullPtr 
    then die msg
    else return ()

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
    ctxt <- GccJit.contextAcquire
    dieIfNull ctxt "NULL ctxt"
   
    -- Set some options on the context.
    -- Let's see the code being generated, in assembler form.
    GccJit.setBoolOption ctxt GccJit.DumpGeneratedCode True

    -- Populate the context.
    createCode ctxt

    -- Compile the code.
    result <- GccJit.contextCompile ctxt
    dieIfNull result "NULL result" 

    -- Extract the generated code from "result".
    greet <- GccJit.resultGetCode result "greet"
    -- Now call the generated function:
    case greet of
        Just greet' -> mkFun greet' <$> newCString "world"
        Nothing -> die "NULL greet"
    hFlush stdout

    GccJit.contextRelease ctxt
    GccJit.resultRelease result


