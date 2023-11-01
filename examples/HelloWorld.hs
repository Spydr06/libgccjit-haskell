{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import qualified GccJit

import Foreign.Ptr
import Foreign.C (CString, newCString)
import System.Exit
import System.IO

type GreetFunction = CString -> IO ()
foreign import ccall "dynamic" mkFun :: FunPtr GreetFunction -> GreetFunction

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
        void greet (const char *name)
        {
            printf ("hello %s\n", name);
        }
    -}
    voidType <- GccJit.contextGetType ctxt GccJit.Void
    constCharPtrType <- GccJit.contextGetType ctxt GccJit.ConstCharPtr
    paramName <- GccJit.contextNewParam ctxt nullPtr constCharPtrType "format"
    func <- GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionExported voidType "greet" [paramName] False

    paramFormat <- GccJit.contextNewParam ctxt nullPtr constCharPtrType "format"
    intType <- GccJit.contextGetType ctxt GccJit.Int
    printFunc <- GccJit.contextNewFunction ctxt nullPtr GccJit.FunctionImported intType "printf" [paramFormat] True

    formatArg <- GccJit.contextNewStringLiteral ctxt "Hello %s!\n"
    nameArg <- GccJit.paramAsRValue paramName

    block <- unwrapOrDie (GccJit.functionNewBlock func Nothing) "NULL block"
    printCall <- GccJit.contextNewCall ctxt nullPtr printFunc [formatArg, nameArg]

    GccJit.blockAddEval block nullPtr printCall
    GccJit.blockEndWithVoidReturn block nullPtr

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
    worldStr <- newCString "World"
    mkFun greet worldStr
    hFlush stdout

    GccJit.contextRelease ctxt
    GccJit.resultRelease result


