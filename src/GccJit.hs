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
    Timer,
    -- enums
    StrOption(..),
    IntOption(..),
    BoolOption(..),
    OutputKind(..),
    Types(..),
    FunctionKind(..),
    TLSModel(..),
    GlobalKind(..),
    UnaryOp(..),
    BinaryOp(..),
    Comparison(..),
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
    fieldAsObject,
    contextNewStructType,
    contextNewOpaqueStruct,
    structAsType,
    structSetFields,
    structGetField,
    structGetFieldCount,
    contextNewUnionType,
    contextNewFunctionPtrType,
    contextNewParam,
    paramAsObject,
    paramAsLValue,
    paramAsRValue,
    contextNewFunction,
    contextGetBuiltinFunction,
    functionAsObject,
    functionGetParam,
    functionDumpToDot,
    functionNewBlock,
    blockAsObject,
    blockGetFunction,
    contextNewGlobal,
    contextNewStructConstructor,
    contextNewUnionConstructor,
    contextNewArrayConstructor,
    globalSetInitializerRValue,
    globalSetInitializer,
    lValueAsObject,
    lValueAsRValue,
    rValueAsObject,
    rValueGetType,
    contextZero,
    contextOne,
    contextNewRValueFromInt,
    contextNewRValueFromLong,
    contextNewRValueFromDouble,
    contextNewRValueFromPtr,
    contextNull,
    contextNewStringLiteral,
    contextNewUnaryOp,
    contextNewBinaryOp,
    contextNewComparison,
    contextNewCall,
    contextNewCallThroughPtr,
    contextNewCast,
    contextNewBitcast,
    lValueGetAlignemnt,
    lValueSetAlignment,
    contextNewArrayAccess,
    lValueAccessField,
    rValueAccessField,
    rValueDereferenceField,
    rValueDereference,
    lValueGetAddress,
    lValueSetLinkSection,
    lValueSetTLSModel,
    lValueSetRegisterName,
    functionNewLocal,
    blockAddEval,
    blockAddAssignment,
    blockAddAssignmentOp,
    blockAddComment,
    blockEndWithConditional,
    blockEndWithJump,
    blockEndWithReturn,
    blockEndWithVoidReturn,
    contextNewCase,
    caseAsObject,
    blockEndWithSwitch,
    contextNewChildContext,
    contextDumpReproducerToFile,
    contextEnableDump,
    timerNew,
    timerRelease,
    contextSetTimer,
    contextGetTimer,
    timerPush,
    timerPop,
    timerPrint,
    rValueSetBoolRequireTailCall,
    typeGetAligned,
    typeGetVector,
    functionGetAddress,
    contextNewRValueFromVector,
    versionMajor,
    versionMinor,
    versionPatchlevel,
) where

import Foreign
import Foreign.C.Types
import Foreign.C (CString, newCString)
import System.Posix.Types (CSsize)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Applicative (Applicative(liftA2))
import System.Timeout (Timeout)
import GHC.Read (list)

-- libgccjit data types (opaque C structs)
data Context
data Result
data Object
data Location
data Type
data Field
data Struct
data FunctionType
data VectorType
data Function
data Block
data RValue
data LValue
data Param
data Case
data ExtendedAsm

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe ptr | ptr == nullPtr = Nothing
               | otherwise = Just ptr

maybeToPtr :: Maybe (Ptr a) -> Ptr a
maybeToPtr Nothing = nullPtr
maybeToPtr (Just p) = p

listToPtr :: Storable a => [a] -> IO (Ptr a)
listToPtr xs = withArray xs return

unzipPairs :: [(a, b)] -> ([a], [b])
unzipPairs xs = (fst <$> xs, snd <$> xs)

foreign import ccall "gcc_jit_context_acquire" gcc_jit_context_acquire :: IO (Ptr Context)
contextAcquire :: IO (Maybe (Ptr Context))
contextAcquire = ptrToMaybe <$> gcc_jit_context_acquire

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

foreign import ccall "gcc_jit_context_compile" gcc_jit_context_compile :: Ptr Context -> IO (Ptr Result)
contextCompile :: Ptr Context -> IO (Maybe (Ptr Result))
contextCompile = fmap ptrToMaybe . gcc_jit_context_compile

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

foreign import ccall "gcc_jit_context_new_struct_type" gcc_jit_context_new_struct_type :: Ptr Context -> Ptr Location -> CString -> CInt -> Ptr (Ptr Field) -> IO (Ptr Struct)
contextNewStructType :: Ptr Context -> Ptr Location -> String -> [Ptr Field] -> IO (Ptr Struct)
contextNewStructType ctxt loc name fields = do
    c_name <- newCString name
    c_fields <- listToPtr fields
    gcc_jit_context_new_struct_type ctxt loc c_name (fromIntegral $ length fields) c_fields 

foreign import ccall "gcc_jit_context_new_opaque_struct" gcc_jit_context_new_opaque_struct :: Ptr Context -> Ptr Location -> CString -> IO (Ptr Struct)
contextNewOpaqueStruct :: Ptr Context -> Ptr Location -> String -> IO (Ptr Struct)
contextNewOpaqueStruct ctxt loc name = do
    c_name <- newCString name
    gcc_jit_context_new_opaque_struct ctxt loc c_name

foreign import ccall "gcc_jit_struct_as_type" structAsType :: Ptr Struct -> Ptr Type

foreign import ccall "gcc_jit_struct_set_fields" gcc_jit_struct_set_fields :: Ptr Struct -> Ptr Location -> CInt -> Ptr (Ptr Field) -> IO ()
structSetFields :: Ptr Struct -> Ptr Location -> [Ptr Field] -> IO ()
structSetFields struct loc fields = do
    c_fields <- listToPtr fields
    gcc_jit_struct_set_fields struct loc (fromIntegral $ length fields) c_fields

foreign import ccall "gcc_jit_struct_get_field" gcc_jit_struct_get_field :: Ptr Struct -> CSize -> IO (Ptr Field)
structGetField :: Ptr Struct -> Int -> IO (Maybe (Ptr Field))
structGetField struct index = fmap ptrToMaybe . gcc_jit_struct_get_field struct $ fromIntegral index

foreign import ccall "gcc_jit_struct_get_field_count" gcc_jit_struct_get_field_count :: Ptr Struct -> IO CSize
structGetFieldCount :: Ptr Struct -> IO Int
structGetFieldCount = fmap fromIntegral . gcc_jit_struct_get_field_count

foreign import ccall "gcc_jit_context_new_union_type" gcc_jit_context_new_union_type :: Ptr Context -> Ptr Location -> CString -> CInt -> Ptr (Ptr Field) -> IO (Ptr Type)
contextNewUnionType :: Ptr Context -> Ptr Location -> String -> [Ptr Field] -> IO (Ptr Type)
contextNewUnionType ctxt loc name fields = do
    c_name <- newCString name
    c_fields <- listToPtr fields
    gcc_jit_context_new_union_type ctxt loc c_name (fromIntegral $ length fields) c_fields

foreign import ccall "gcc_jit_context_new_function_ptr_type" gcc_jit_context_new_function_ptr_type :: Ptr Context -> Ptr Location -> Ptr Type -> CInt -> Ptr (Ptr Type) -> CInt -> IO (Ptr Type)
contextNewFunctionPtrType :: Ptr Context -> Ptr Location -> Ptr Type -> [Ptr Type] -> Bool -> IO (Ptr Type)
contextNewFunctionPtrType ctxt loc returnType paramTypes isVariadic = do
    c_paramTypes <- listToPtr paramTypes
    gcc_jit_context_new_function_ptr_type ctxt loc returnType (fromIntegral $ length paramTypes) c_paramTypes $ fromIntegral $ fromEnum isVariadic

-- Constructing functions.

foreign import ccall "gcc_jit_context_new_param" gcc_jit_context_new_param :: Ptr Context -> Ptr Location -> Ptr Type -> CString -> IO (Ptr Param)
contextNewParam :: Ptr Context -> Ptr Location -> Ptr Type -> String -> IO (Ptr Param)
contextNewParam ctxt loc type' name = do
    c_name <- newCString name
    gcc_jit_context_new_param ctxt loc type' c_name

foreign import ccall "gcc_jit_param_as_object" paramAsObject :: Ptr Param -> IO (Ptr Object)
foreign import ccall "gcc_jit_param_as_lvalue" paramAsLValue :: Ptr Param -> IO (Ptr LValue)
foreign import ccall "gcc_jit_param_as_rvalue" paramAsRValue :: Ptr Param -> IO (Ptr RValue)

data FunctionKind = FunctionExported
    | FunctionInternal
    | FunctionImported
    | FunctionAlwaysInline
    deriving (Enum)

data TLSModel = TSLNone
    | TLSGlobalDynamic
    | TLSLocalDynamic
    | TLSInitialExec
    | TLSLocalExec
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_function" gcc_jit_context_new_function :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> CString -> CInt -> Ptr (Ptr Param) -> CInt -> IO (Ptr Function)
contextNewFunction :: Ptr Context -> Ptr Location -> FunctionKind -> Ptr Type -> String -> [Ptr Param] -> Bool -> IO (Ptr Function)
contextNewFunction ctxt loc kind returnType name params isVariadic = do
    c_name <- newCString name
    c_params <- listToPtr params
    gcc_jit_context_new_function ctxt loc (fromIntegral $ fromEnum kind) returnType c_name (fromIntegral $ length params) c_params $ fromIntegral $ fromEnum isVariadic

foreign import ccall "gcc_jit_context_get_builtin_function" gcc_jit_context_get_builtin_function :: Ptr Context -> CString -> IO (Ptr Function)
contextGetBuiltinFunction :: Ptr Context -> String -> IO (Maybe (Ptr Function))
contextGetBuiltinFunction ctxt name = do
    c_name <- newCString name
    ptrToMaybe <$> gcc_jit_context_get_builtin_function ctxt c_name

foreign import ccall "gcc_jit_function_as_object" functionAsObject :: Ptr Function -> IO (Ptr Object)

foreign import ccall "gcc_jit_function_get_param" gcc_jit_function_get_param :: Ptr Function -> CInt -> IO (Ptr Object)
functionGetParam :: Ptr Function -> Int -> IO (Maybe (Ptr Object))
functionGetParam func = fmap ptrToMaybe . gcc_jit_function_get_param func . fromIntegral

foreign import ccall "gcc_jit_function_dump_to_dot" gcc_jit_function_dump_to_dot :: Ptr Function -> CString -> IO ()
functionDumpToDot :: Ptr Function -> String -> IO ()
functionDumpToDot func path = do
    c_path <- newCString path
    gcc_jit_function_dump_to_dot func c_path

foreign import ccall "gcc_jit_function_new_block" gcc_jit_function_new_block :: Ptr Function -> CString -> IO (Ptr Block)
functionNewBlock :: Ptr Function -> Maybe String -> IO (Maybe (Ptr Block))
functionNewBlock func (Just name) = do
    c_name <- newCString name
    ptrToMaybe <$> gcc_jit_function_new_block func c_name
functionNewBlock func Nothing = ptrToMaybe <$> gcc_jit_function_new_block func nullPtr

foreign import ccall "gcc_jit_block_as_object" blockAsObject :: Ptr Block -> IO (Ptr Object)

foreign import ccall "gcc_jit_block_get_function" blockGetFunction :: Ptr Block -> IO (Ptr Function)

data GlobalKind = GlobalExported
    | GlobalInternal
    | GlobalImported
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_global" gcc_jit_context_new_global :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> CString -> IO (Ptr LValue)
contextNewGlobal :: Ptr Context -> Ptr Location -> GlobalKind -> Ptr Type -> String -> IO (Ptr LValue)
contextNewGlobal ctxt loc kind type' name = do
    c_name <- newCString name
    gcc_jit_context_new_global ctxt loc (fromIntegral $ fromEnum kind) type' c_name

foreign import ccall "gcc_jit_context_new_struct_constructor" gcc_jit_context_new_struct_constructor :: Ptr Context -> Ptr Location -> Ptr Type -> CSize -> Ptr (Ptr Field) -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewStructConstructor :: Ptr Context -> Ptr Location -> Ptr Type -> Either [Ptr RValue] [(Ptr Field, Maybe (Ptr RValue))] -> IO (Maybe (Ptr RValue))
contextNewStructConstructor ctxt loc type' (Left values) = do
    c_values <- listToPtr values
    ptrToMaybe <$> gcc_jit_context_new_struct_constructor ctxt loc type' (fromIntegral $ length values) nullPtr c_values
contextNewStructConstructor ctxt loc type' (Right fieldValues) = do
    (c_fields, c_values) <- uncurry (liftA2 (,)) $ bimap listToPtr (listToPtr . map maybeToPtr) $ unzipPairs fieldValues 
    ptrToMaybe <$> gcc_jit_context_new_struct_constructor ctxt loc type' (fromIntegral $ length fieldValues) c_fields c_values

foreign import ccall "gcc_jit_context_new_union_constructor" gcc_jit_context_new_union_constructor :: Ptr Context -> Ptr Location -> Ptr Type -> Ptr Field -> Ptr RValue -> IO (Ptr RValue)
contextNewUnionConstructor :: Ptr Context -> Ptr Location -> Ptr Type -> Maybe (Ptr Field) -> Maybe (Ptr RValue) -> IO (Maybe (Ptr RValue))
contextNewUnionConstructor ctxt loc type' field value = 
    ptrToMaybe <$> gcc_jit_context_new_union_constructor ctxt loc type' (maybeToPtr field) (maybeToPtr value)

foreign import ccall "gcc_jit_context_new_array_constructor" gcc_jit_context_new_array_constructor :: Ptr Context -> Ptr Location -> Ptr Type -> CSize -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewArrayConstructor :: Ptr Context -> Ptr Location -> Ptr Type -> [Ptr RValue] -> IO (Maybe (Ptr RValue))
contextNewArrayConstructor ctxt loc type' values = do
    c_values <- listToPtr values
    ptrToMaybe <$> gcc_jit_context_new_array_constructor ctxt loc type' (fromIntegral $ length values) c_values

foreign import ccall "gcc_jit_global_set_initializer_rvalue" gcc_jit_global_set_initializer_rvalue :: Ptr LValue -> Ptr RValue -> IO (Ptr LValue)
globalSetInitializerRValue :: Ptr LValue -> Ptr RValue -> IO (Maybe (Ptr LValue))
globalSetInitializerRValue global = fmap ptrToMaybe . gcc_jit_global_set_initializer_rvalue global

foreign import ccall "gcc_jit_global_set_initializer" gcc_jit_global_set_initializer :: Ptr LValue -> Ptr a -> CSize -> IO (Ptr LValue)
globalSetInitializer :: Storable a => Ptr LValue -> Ptr a -> Int -> IO (Ptr LValue)
globalSetInitializer global ptr = gcc_jit_global_set_initializer global ptr . fromIntegral

foreign import ccall "gcc_jit_lvalue_as_object" lValueAsObject :: Ptr LValue -> IO (Ptr Object)
foreign import ccall "gcc_jit_rvalue_as_object" rValueAsObject :: Ptr RValue -> IO (Ptr Object)
foreign import ccall "gcc_jit_lvalue_as_rvalue" lValueAsRValue :: Ptr LValue -> IO (Ptr RValue)
foreign import ccall "gcc_jit_rvalue_get_type" rValueGetType :: Ptr RValue -> IO (Ptr Type)

foreign import ccall "gcc_jit_context_zero" contextZero :: Ptr Context -> Ptr Type -> IO (Ptr RValue)
foreign import ccall "gcc_jit_context_one" contextOne :: Ptr Context -> Ptr Type -> IO (Ptr RValue)

foreign import ccall "gcc_jit_context_new_rvalue_from_int" gcc_jit_context_new_rvalue_from_int :: Ptr Context -> Ptr Type -> CInt -> IO (Ptr RValue)
contextNewRValueFromInt :: Ptr Context -> Ptr Type -> Int -> IO (Ptr RValue)
contextNewRValueFromInt ctxt numericType = gcc_jit_context_new_rvalue_from_int ctxt numericType . fromIntegral

foreign import ccall "gcc_jit_context_new_rvalue_from_long" gcc_jit_context_new_rvalue_from_long :: Ptr Context -> Ptr Type -> CLong -> IO (Ptr RValue)
contextNewRValueFromLong :: Ptr Context -> Ptr Type -> Int64 -> IO (Ptr RValue)
contextNewRValueFromLong ctxt numericType = gcc_jit_context_new_rvalue_from_long ctxt numericType . fromIntegral

foreign import ccall "gcc_jit_context_new_rvalue_from_double" gcc_jit_context_new_rvalue_from_double :: Ptr Context -> Ptr Type -> CDouble -> IO (Ptr RValue)
contextNewRValueFromDouble :: Ptr Context -> Ptr Type -> Double -> IO (Ptr RValue)
contextNewRValueFromDouble ctxt numericType = gcc_jit_context_new_rvalue_from_double ctxt numericType . realToFrac

foreign import ccall "gcc_jit_context_new_rvalue_from_ptr" contextNewRValueFromPtr :: Ptr Context -> Ptr Type -> Ptr a -> IO (Ptr RValue)
foreign import ccall "gcc_jit_context_null" contextNull :: Ptr Context -> Ptr Type -> IO (Ptr RValue)

foreign import ccall "gcc_jit_context_new_string_literal" gcc_jit_context_new_string_literal :: Ptr Context -> CString -> IO (Ptr RValue)
contextNewStringLiteral :: Ptr Context -> String -> IO (Ptr RValue)
contextNewStringLiteral ctxt value = do
    c_value <- newCString value
    gcc_jit_context_new_string_literal ctxt c_value

data UnaryOp = UnaryMinus
    | UnaryBitwiseNegate
    | UnaryLogicalNegate
    | UnaryAbs
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_unary_op" gcc_jit_context_new_unary_op :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> Ptr RValue -> IO (Ptr RValue)
contextNewUnaryOp :: Ptr Context -> Ptr Location -> UnaryOp -> Ptr Type -> Ptr RValue -> IO (Ptr RValue)
contextNewUnaryOp ctxt loc op = gcc_jit_context_new_unary_op ctxt loc $ fromIntegral $ fromEnum op

data BinaryOp = BinaryPlus
    | BinaryMinus
    | BinaryMult
    | BinaryDivide
    | BinaryModulo
    | BinaryBitwiseAnd
    | BinaryBitwiseXor
    | BinaryBitwiseOr
    | BinaryLogicalAnd
    | BinaryLogicalOr
    | BinaryLShift
    | BinaryRShift
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_binary_op" gcc_jit_context_new_binary_op :: Ptr Context -> Ptr Location -> CInt -> Ptr Type -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewBinaryOp :: Ptr Context -> Ptr Location -> BinaryOp -> Ptr Type -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewBinaryOp ctxt loc op = gcc_jit_context_new_binary_op ctxt loc $ fromIntegral $ fromEnum op

data Comparison = ComparisonEq
    | ComparisonNe
    | ComparisonLt
    | ComparisonLe
    | ComparisonGt
    | ComparisonGe
    deriving (Enum)

foreign import ccall "gcc_jit_context_new_comparison" gcc_jit_context_new_comparison :: Ptr Context -> Ptr Location -> CInt -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewComparison :: Ptr Context -> Ptr Location -> Comparison -> Ptr RValue -> Ptr RValue -> IO (Ptr RValue)
contextNewComparison ctxt  loc op = gcc_jit_context_new_comparison ctxt loc $ fromIntegral $ fromEnum op

foreign import ccall "gcc_jit_context_new_call" gcc_jit_context_new_call :: Ptr Context -> Ptr Location -> Ptr Function -> CInt -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewCall :: Ptr Context -> Ptr Location -> Ptr Function -> [Ptr RValue] -> IO (Ptr RValue)
contextNewCall ctxt loc func args = do
    c_args <- listToPtr args
    gcc_jit_context_new_call ctxt loc func (fromIntegral $ length args) c_args

foreign import ccall "gcc_jit_context_new_call_through_ptr" gcc_jit_context_new_call_through_ptr :: Ptr Context -> Ptr Location -> Ptr RValue -> CInt -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewCallThroughPtr :: Ptr Context -> Ptr Location -> Ptr RValue -> [Ptr RValue] -> IO (Ptr RValue)
contextNewCallThroughPtr ctxt loc fnPtr args = do
    c_args <- listToPtr args
    gcc_jit_context_new_call_through_ptr ctxt loc fnPtr (fromIntegral $ length args) c_args

foreign import ccall "gcc_jit_context_new_cast" contextNewCast :: Ptr Context -> Ptr Location -> Ptr RValue -> Ptr Type -> IO (Ptr RValue)
foreign import ccall "gcc_jit_context_new_bitcast" contextNewBitcast :: Ptr Context -> Ptr Location -> Ptr RValue -> Ptr Type -> IO (Ptr RValue)

foreign import ccall "gcc_jit_lvalue_set_alignment" gcc_jit_lvalue_set_alignment :: Ptr LValue -> CUInt -> IO ()
lValueSetAlignment :: Ptr LValue -> Int -> IO ()
lValueSetAlignment lvalue = gcc_jit_lvalue_set_alignment lvalue . fromIntegral

foreign import ccall "gcc_jit_lvalue_get_alignment" gcc_jit_lvalue_get_alignment :: Ptr LValue -> IO CUInt
lValueGetAlignemnt :: Ptr LValue -> IO Int
lValueGetAlignemnt = fmap fromIntegral . gcc_jit_lvalue_get_alignment

foreign import ccall "gcc_jit_context_new_array_access" contextNewArrayAccess :: Ptr Context -> Ptr Location -> Ptr RValue -> Ptr RValue -> IO (Ptr LValue)
foreign import ccall "gcc_jit_lvalue_access_field" lValueAccessField :: Ptr LValue -> Ptr Location -> Ptr Field -> IO (Ptr LValue)
foreign import ccall "gcc_jit_rvalue_access_field" rValueAccessField :: Ptr RValue -> Ptr Location -> Ptr Field -> IO (Ptr RValue)
foreign import ccall "gcc_jit_rvalue_dereference_field" rValueDereferenceField :: Ptr RValue -> Ptr Location -> Ptr Field -> IO (Ptr LValue)
foreign import ccall "gcc_jit_rvalue_dereference" rValueDereference :: Ptr RValue -> Ptr Location -> IO (Ptr LValue)
foreign import ccall "gcc_jit_lvalue_get_address" lValueGetAddress :: Ptr LValue -> Ptr Location -> IO (Ptr RValue)

foreign import ccall "gcc_jit_lvalue_set_tls_model" gcc_jit_lvalue_set_tls_model :: Ptr LValue -> CInt -> IO ()
lValueSetTLSModel :: Ptr LValue -> TLSModel -> IO ()
lValueSetTLSModel lvalue = gcc_jit_lvalue_set_tls_model lvalue . fromIntegral . fromEnum

foreign import ccall "gcc_jit_lvalue_set_link_section" gcc_jit_lvalue_set_link_section :: Ptr LValue -> CString -> IO ()
lValueSetLinkSection :: Ptr LValue -> String -> IO ()
lValueSetLinkSection lvalue sectionName = do
    c_sectionName <- newCString sectionName
    gcc_jit_lvalue_set_link_section lvalue c_sectionName

foreign import ccall "gcc_jit_lvalue_set_register_name" gcc_jit_lvalue_set_register_name :: Ptr LValue -> CString -> IO ()
lValueSetRegisterName :: Ptr LValue -> String -> IO ()
lValueSetRegisterName lvalue registerName = do
    c_registerName <- newCString registerName
    gcc_jit_lvalue_set_register_name lvalue c_registerName

foreign import ccall "gcc_jit_function_new_local" gcc_jit_function_new_local :: Ptr Function -> Ptr Location -> Ptr Type -> CString -> IO (Ptr LValue)
functionNewLocal :: Ptr Function -> Ptr Location -> Ptr Type -> String -> IO (Ptr LValue)
functionNewLocal func loc type' name = do
    c_name <- newCString name
    gcc_jit_function_new_local func loc type' c_name

foreign import ccall "gcc_jit_block_add_eval" blockAddEval :: Ptr Block -> Ptr Location -> Ptr RValue -> IO ()
foreign import ccall "gcc_jit_block_add_assignment" blockAddAssignment :: Ptr Block -> Ptr Location -> Ptr LValue -> Ptr RValue -> IO ()

foreign import ccall "gcc_jit_block_add_assignment_op" gcc_jit_block_add_assignment_op :: Ptr Block -> Ptr Location -> Ptr LValue -> CInt -> Ptr RValue -> IO ()
blockAddAssignmentOp :: Ptr Block -> Ptr Location -> Ptr LValue -> BinaryOp -> Ptr RValue -> IO ()
blockAddAssignmentOp block loc lvalue = gcc_jit_block_add_assignment_op block loc lvalue . fromIntegral . fromEnum

foreign import ccall "gcc_jit_block_add_comment" gcc_jit_block_add_comment :: Ptr Block -> Ptr Location -> CString -> IO ()
blockAddComment :: Ptr Block -> Ptr Location -> String -> IO ()
blockAddComment block loc text = do
    c_text <- newCString text
    gcc_jit_block_add_comment block loc c_text

foreign import ccall "gcc_jit_block_end_with_conditional" blockEndWithConditional :: Ptr Block -> Ptr Location -> Ptr RValue -> Ptr Block -> Ptr Block -> IO ()
foreign import ccall "gcc_jit_block_end_with_jump" blockEndWithJump :: Ptr Block -> Ptr Location -> Ptr Block -> IO ()
foreign import ccall "gcc_jit_block_end_with_return" blockEndWithReturn :: Ptr Block -> Ptr Location -> Ptr RValue -> IO ()
foreign import ccall "gcc_jit_block_end_with_void_return" blockEndWithVoidReturn :: Ptr Block -> Ptr Location -> IO ()

foreign import ccall "gcc_jit_context_new_case" contextNewCase :: Ptr Context -> Ptr RValue -> Ptr RValue -> Ptr Block -> IO (Ptr Case)
foreign import ccall "gcc_jit_case_as_object" caseAsObject :: Ptr Case -> IO (Ptr Object)

foreign import ccall "gcc_jit_block_end_with_switch" gcc_jit_block_end_with_switch :: Ptr Block -> Ptr Location -> Ptr RValue -> Ptr Block -> CInt -> Ptr (Ptr Case) -> IO ()
blockEndWithSwitch :: Ptr Block -> Ptr Location -> Ptr RValue -> Ptr Block -> [Ptr Case] -> IO ()
blockEndWithSwitch block loc expr defaultBlock cases = do
    c_cases <- listToPtr cases
    gcc_jit_block_end_with_switch block loc expr defaultBlock (fromIntegral $ length cases) c_cases

foreign import ccall "gcc_jit_context_new_child_context" gcc_jit_context_new_child_context :: Ptr Context -> IO (Ptr Context)
contextNewChildContext :: Ptr Context -> IO (Maybe (Ptr Context))
contextNewChildContext = fmap ptrToMaybe . gcc_jit_context_new_child_context

foreign import ccall "gcc_jit_context_dump_reproducer_to_file" gcc_jit_context_dump_reproducer_to_file :: Ptr Context -> CString -> IO ()
contextDumpReproducerToFile :: Ptr Context -> String -> IO ()
contextDumpReproducerToFile ctxt path = do
    c_path <- newCString path
    gcc_jit_context_dump_reproducer_to_file ctxt c_path

foreign import ccall "gcc_jit_context_enable_dump" gcc_jit_context_enable_dump :: Ptr Context -> CString -> Ptr CString -> IO ()
contextEnableDump :: Ptr Context -> String -> Ptr CString -> IO ()
contextEnableDump ctxt dumpName outPtr = do
    c_dumpName <- newCString dumpName
    gcc_jit_context_enable_dump ctxt c_dumpName outPtr

data Timer

foreign import ccall "gcc_jit_timer_new" timerNew :: IO (Ptr Timer)
foreign import ccall "gcc_jit_timer_release" timerRelease :: Ptr Timer -> IO ()
foreign import ccall "gcc_jit_context_set_timer" contextSetTimer :: Ptr Context -> Ptr Timer -> IO ()

foreign import ccall "gcc_jit_context_get_timer" gcc_jit_context_get_timer :: Ptr Context -> IO (Ptr Timer)
contextGetTimer :: Ptr Context -> IO (Maybe (Ptr Timer))
contextGetTimer = fmap ptrToMaybe . gcc_jit_context_get_timer

foreign import ccall "gcc_jit_timer_push" gcc_jit_timer_push :: Ptr Timer -> CString -> IO ()
timerPush :: Ptr Timer -> String -> IO ()
timerPush timer itemName = do
    c_itemName <- newCString itemName
    gcc_jit_timer_push timer c_itemName

foreign import ccall "gcc_jit_timer_pop" gcc_jit_timer_pop :: Ptr Timer -> CString -> IO ()
timerPop :: Ptr Timer -> String -> IO ()
timerPop timer itemName = do
    c_itemName <- newCString itemName
    gcc_jit_timer_pop timer c_itemName

foreign import ccall "gcc_jit_timer_print" timerPrint :: Ptr Timer -> Ptr CFile -> IO ()

foreign import ccall "gcc_jit_rvalue_set_bool_require_tail_call" gcc_jit_rvalue_set_bool_require_tail_call :: Ptr RValue -> CInt -> IO ()
rValueSetBoolRequireTailCall :: Ptr RValue -> Bool -> IO ()
rValueSetBoolRequireTailCall call = gcc_jit_rvalue_set_bool_require_tail_call call . fromIntegral . fromEnum

foreign import ccall "gcc_jit_type_get_aligned" gcc_jit_type_get_aligned :: Ptr Type -> CSize -> IO (Ptr Type)
typeGetAligned :: Ptr Type -> Int64 -> IO (Ptr Type)
typeGetAligned type' = gcc_jit_type_get_aligned type' . fromIntegral

foreign import ccall "gcc_jit_type_get_vector" gcc_jit_type_get_vector :: Ptr Type -> CSize -> IO (Ptr Type)
typeGetVector :: Ptr Type -> Int64 -> IO (Ptr Type)
typeGetVector type' = gcc_jit_type_get_vector type' . fromIntegral

foreign import ccall "gcc_jit_function_get_address" functionGetAddress :: Ptr Function -> Ptr Location -> Ptr RValue

foreign import ccall "gcc_jit_context_new_rvalue_from_vector" gcc_jit_context_new_rvalue_from_vector :: Ptr Context -> Ptr Location -> Ptr Type -> CSize -> Ptr (Ptr RValue) -> IO (Ptr RValue)
contextNewRValueFromVector :: Ptr Context -> Ptr Location -> Ptr Type -> [Ptr RValue] -> IO (Ptr RValue)
contextNewRValueFromVector ctxt loc vecType elements = do
    c_elements <- listToPtr elements
    gcc_jit_context_new_rvalue_from_vector ctxt loc vecType (fromIntegral $ length elements) c_elements

foreign import ccall "gcc_jit_version_major" gcc_jit_version_major :: IO CInt
versionMajor :: IO Int
versionMajor = fromIntegral <$> gcc_jit_version_major

foreign import ccall "gcc_jit_version_minor" gcc_jit_version_minor :: IO CInt
versionMinor :: IO Int
versionMinor = fromIntegral <$> gcc_jit_version_minor

foreign import ccall "gcc_jit_version_patchlevel" gcc_jit_version_patchlevel :: IO CInt
versionPatchlevel :: IO Int
versionPatchlevel = fromIntegral <$> gcc_jit_version_patchlevel


