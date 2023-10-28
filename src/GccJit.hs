{-# LANGUAGE ForeignFunctionInterface #-}

module GccJit (
    -- C structs
    Context,
    Result,
    Object,
    Location,
    Type,
    Field,
    Struct,
    FunctionType,
    VectorType,
    Function,
    Block,
    RValue,
    LValue,
    Param,
    Case,
    ExtendedAsm,
    -- enums
    StrOption(..),
    IntOption(..),
    BoolOption(..),
    OutputKind(..),
    Types(..),
    -- functions
    contextAcquire,
    contextRelease,
    setStrOption,
    setIntOption,
    setBoolOption,
    contextSetBoolAllowUnreachableBlocks,
    contextSetBoolPrintErrorsToStderr,
    contextSetBoolUseExternalDriver,
    contextAddCommandLineOption,
    contextAddDriverOption,
    contextCompile,
    contextCompileToFile,
    contextDumpToFile,
    contextSetLogfile,
    contextGetFirstError,
    contextGetLastError,
    resultGetCode,
    resultGetGlobal,
    resultRelease,
    objectGetContext,
    objectGetDebugString,
    contextNewLocation,
    locationAsObject,
    typeAsObject,
    contextGetType,
    contextGetIntType,
    typeGetPointer,
    typeGetConst,
    typeGetVolatile,
    compatibleTypes,
    typeGetSize,
    contextNewArrayType,
    contextNewField,
    contextNewBitfield,
    fieldAsObject
) where

import Foreign
import Foreign.C.Types
import Foreign.C (CString, newCString)
import System.Posix.Types (CSsize)

-- libgccjit data types (opaque C structs)
data Context = Context
data Result = Result
data Object = Object
data Location = Location
data Type = Type
data Field = Field
data Struct = Struct
data FunctionType = FunctionType
data VectorType = VectorType
data Function = Function
data Block = Block
data RValue = RValue
data LValue = LValue
data Param = Param
data Case = Case
data ExtendedAsm = ExtendedAsm

foreign import ccall "gcc_jit_context_acquire" contextAcquire :: IO (Ptr Context)
foreign import ccall "gcc_jit_context_release" contextRelease :: Ptr Context -> IO ()

data StrOption = Progname 
    | NumStrOptions
    deriving (Enum)

data IntOption = OptimizationLevel
    | NumIntOptions
    deriving (Enum)

data BoolOption = DebugInfo
    | DumpInitialTree
    | DumpInitialGimple
    | DumpGeneratedCode
    | DumpSummary
    | DumpEverything
    | SelfcheckGC
    | KeepIntermediate
    | NumBoolOptions
    deriving (Enum)

foreign import ccall "gcc_jit_context_set_str_option" gcc_jit_set_str_option :: Ptr Context -> CInt -> CString -> IO ()
setStrOption :: Ptr Context -> StrOption -> String -> IO ()
setStrOption ctxt opt value = do
    c_value <- newCString value
    gcc_jit_set_str_option ctxt (fromIntegral $ fromEnum opt) c_value

foreign import ccall "gcc_jit_context_set_int_option" gcc_jit_set_int_option :: Ptr Context -> CInt -> CInt -> IO ()
setIntOption :: Ptr Context -> IntOption -> Int -> IO ()
setIntOption ctxt opt = gcc_jit_set_int_option ctxt (fromIntegral $ fromEnum opt) . fromIntegral

foreign import ccall "gcc_jit_context_set_bool_option" gcc_jit_set_bool_option :: Ptr Context -> CInt -> CInt -> IO ()
setBoolOption :: Ptr Context -> BoolOption -> Bool -> IO ()
setBoolOption ctxt opt = gcc_jit_set_bool_option ctxt (fromIntegral $ fromEnum opt) . fromIntegral . fromEnum

foreign import ccall "gcc_jit_context_set_bool_allow_unreachable_blocks" gcc_jit_context_set_bool_allow_unreachable_blocks :: Ptr Context -> CInt -> IO ()
contextSetBoolAllowUnreachableBlocks :: Ptr Context -> Bool -> IO ()
contextSetBoolAllowUnreachableBlocks ctxt = gcc_jit_context_set_bool_allow_unreachable_blocks ctxt . fromIntegral . fromEnum

foreign import ccall "gcc_jit_context_set_bool_print_errors_to_stderr" gcc_jit_context_set_bool_print_errors_to_stderr :: Ptr Context -> CInt -> IO ()
contextSetBoolPrintErrorsToStderr :: Ptr Context -> Bool -> IO ()
contextSetBoolPrintErrorsToStderr ctxt = gcc_jit_context_set_bool_print_errors_to_stderr ctxt . fromIntegral . fromEnum

foreign import ccall "gcc_jit_context_set_bool_use_external_driver" gcc_jit_context_set_bool_use_external_driver :: Ptr Context -> CInt -> IO ()
contextSetBoolUseExternalDriver :: Ptr Context -> Bool -> IO ()
contextSetBoolUseExternalDriver ctxt = gcc_jit_context_set_bool_use_external_driver ctxt . fromIntegral . fromEnum

foreign import ccall "gcc_jit_context_add_command_line_option" gcc_jit_context_add_command_line_option :: Ptr Context -> CString -> IO ()
contextAddCommandLineOption :: Ptr Context -> String -> IO ()
contextAddCommandLineOption ctxt optname = do
    c_optname <- newCString optname
    gcc_jit_context_add_command_line_option ctxt c_optname

foreign import ccall "gcc_jit_context_add_driver_option" gcc_jit_context_add_driver_option :: Ptr Context -> CString -> IO ()
contextAddDriverOption :: Ptr Context -> String -> IO ()
contextAddDriverOption ctxt optname = do
    c_optname <- newCString optname
    gcc_jit_context_add_driver_option ctxt c_optname

foreign import ccall "gcc_jit_context_compile" contextCompile :: Ptr Context -> IO (Ptr Result)

data OutputKind = Assembler
    | ObjectFile
    | DynamicLibrary
    | Executable
    deriving (Enum)

foreign import ccall "gcc_jit_context_compile_to_file" gcc_jit_context_compile_to_file :: Ptr Context -> CInt -> CString -> IO ()
contextCompileToFile :: Ptr Context -> OutputKind -> String -> IO ()
contextCompileToFile ctxt output_kind output_path = do
    c_output_path <- newCString output_path
    gcc_jit_context_compile_to_file ctxt (fromIntegral $ fromEnum output_kind) c_output_path

foreign import ccall "gcc_jit_context_dump_to_file" gcc_jit_context_dump_to_file :: Ptr Context -> CString -> CInt -> IO ()
contextDumpToFile :: Ptr Context -> String -> Int -> IO ()
contextDumpToFile ctxt path update_locations = do
    c_path <- newCString path
    gcc_jit_context_dump_to_file ctxt c_path $ fromIntegral $ fromEnum update_locations

foreign import ccall "gcc_jit_context_set_logfile" gcc_jit_context_set_logfile :: Ptr Context -> Ptr CFile -> CInt -> CInt -> IO ()
contextSetLogfile :: Ptr Context -> Ptr CFile -> Int -> Int -> IO ()
contextSetLogfile ctxt logfile flags = gcc_jit_context_set_logfile ctxt logfile (fromIntegral flags) . fromIntegral

foreign import ccall "gcc_jit_context_get_first_error" gcc_jit_context_get_first_error :: Ptr Context -> IO CString
contextGetFirstError :: Ptr Context -> IO (Maybe String)
contextGetFirstError ctxt = do
    c_error <- gcc_jit_context_get_first_error ctxt
    return $ if c_error == nullPtr 
        then Nothing
        else Just $ show c_error

foreign import ccall "gcc_jit_context_get_last_error" gcc_jit_context_get_last_error :: Ptr Context -> IO CString
contextGetLastError :: Ptr Context -> IO (Maybe String)
contextGetLastError ctxt = do
    c_error <- gcc_jit_context_get_last_error ctxt
    return $ if c_error == nullPtr
        then Nothing
        else Just $ show c_error

foreign import ccall "gcc_jit_result_get_code" gcc_jit_result_get_code :: Ptr Result -> CString -> IO (Ptr ())
resultGetCode :: Ptr Result -> String -> IO (Maybe (FunPtr a))
resultGetCode result funcname = do
    c_funcname <- newCString funcname
    c_funptr <- gcc_jit_result_get_code result c_funcname
    return $ if c_funptr == nullPtr
        then Nothing
        else Just $ castPtrToFunPtr c_funptr

foreign import ccall "gcc_jit_result_get_global" gcc_jit_result_get_global :: Ptr Result -> CString -> IO (Ptr ())
resultGetGlobal :: Ptr Result -> String -> IO (Maybe (Ptr a))
resultGetGlobal result name = do
    c_name <- newCString name
    c_ptr <- gcc_jit_result_get_global result c_name
    return $ if c_ptr == nullPtr
        then Nothing
        else Just $ castPtr c_ptr

foreign import ccall "gcc_jit_result_release" resultRelease :: Ptr Result -> IO ()

-- Functions for creating "contextual" objects.

foreign import ccall "gcc_jit_object_get_context" objectGetContext :: IO (Ptr Object)

foreign import ccall "gcc_jit_object_get_debug_string" gcc_jit_object_get_debug_string :: Ptr Object -> IO CString
objectGetDebugString :: Ptr Object -> IO String
objectGetDebugString = fmap show . gcc_jit_object_get_debug_string

foreign import ccall "gcc_jit_context_new_location" gcc_jit_context_new_location :: Ptr Context -> CString -> CInt -> CInt -> IO (Ptr Location)
contextNewLocation :: Ptr Context -> String -> Int -> Int -> IO (Ptr Location)
contextNewLocation ctxt filename line column = do
    c_filename <- newCString filename
    gcc_jit_context_new_location ctxt c_filename (fromIntegral line) $ fromIntegral column

foreign import ccall "gcc_jit_location_as_object" locationAsObject :: Ptr Location -> IO (Ptr Object)

foreign import ccall "gcc_jit_type_as_object" typeAsObject :: Ptr Type -> IO (Ptr Type)

data Types = Void
    | VoidPtr
    | Bool
    | Char
    | SignedChar
    | UnsignedChar
    | Short
    | UnsignedShort
    | Int
    | UnsignedInt
    | Long
    | UnsignedLong
    | LongLong
    | UnsignedLongLong
    | Float
    | Double
    | LongDouble
    | ConstCharPtr
    | SizeT
    | FilePtr
    | ComplexFloat
    | ComplexDouble
    | ComplexLongDouble
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | UInt128
    | Int8
    | Int16
    | Int32
    | Int64
    | Int128
    deriving (Enum)

foreign import ccall "gcc_jit_context_get_type" gcc_jit_context_get_type :: Ptr Context -> CInt -> IO (Ptr Type)
contextGetType :: Ptr Context -> Types -> IO (Ptr Type)
contextGetType ctxt = gcc_jit_context_get_type ctxt . fromIntegral . fromEnum

foreign import ccall "gcc_jit_context_get_int_type" gcc_jit_context_get_int_type :: Ptr Context -> CInt -> CInt -> IO (Ptr Type)
contextGetIntType :: Ptr Context -> Int -> Bool -> IO (Ptr Type)
contextGetIntType ctxt numBytes = gcc_jit_context_get_int_type ctxt (fromIntegral numBytes) . fromIntegral . fromEnum

foreign import ccall "gcc_jit_type_get_pointer" typeGetPointer :: Ptr Type -> IO (Ptr Type)
foreign import ccall "gcc_jit_type_get_const" typeGetConst :: Ptr Type -> IO (Ptr Type)
foreign import ccall "gcc_jit_type_get_volatile" typeGetVolatile :: Ptr Type -> IO (Ptr Type)

foreign import ccall "gcc_jit_compatible_types" gcc_jit_compatible_types :: Ptr Type -> Ptr Type -> IO CInt
compatibleTypes :: Ptr Type -> Ptr Type -> IO Bool
compatibleTypes ltype rtype = (fromIntegral (0 :: Integer) /=) <$> gcc_jit_compatible_types ltype rtype

foreign import ccall "gcc_jit_type_get_size" gcc_jit_type_get_size :: Ptr Type -> IO CSize
typeGetSize :: Ptr Type -> IO CSsize
typeGetSize = fmap fromIntegral . gcc_jit_type_get_size

foreign import ccall "gcc_jit_context_new_array_type" gcc_jit_context_new_array_type :: Ptr Context -> Ptr Location -> Ptr Type -> CInt -> IO (Ptr Type)
contextNewArrayType :: Ptr Context -> Ptr Location -> Ptr Type -> Int -> IO (Ptr Type)
contextNewArrayType ctxt loc elementType = gcc_jit_context_new_array_type ctxt loc elementType . fromIntegral

foreign import ccall "gcc_jit_context_new_field" gcc_jit_context_new_field :: Ptr Context -> Ptr Location -> Ptr Type -> CString -> IO (Ptr Field)
contextNewField :: Ptr Context -> Ptr Location -> Ptr Type -> String -> IO (Ptr Field)
contextNewField ctxt loc type' name = do
    c_name <- newCString name
    gcc_jit_context_new_field ctxt loc type' c_name

foreign import ccall "gcc_jit_context_new_bitfield" gcc_jit_context_new_bitfield :: Ptr Context -> Ptr Location -> Ptr Type -> CInt -> CString -> IO (Ptr Field)
contextNewBitfield :: Ptr Context -> Ptr Location -> Ptr Type -> Int -> String -> IO (Ptr Field)
contextNewBitfield ctxt loc type' width name = do
    c_name <- newCString name
    gcc_jit_context_new_bitfield ctxt loc type' (fromIntegral width) c_name

foreign import ccall "gcc_jit_field_as_object" fieldAsObject :: Ptr Field -> IO (Ptr Object)

