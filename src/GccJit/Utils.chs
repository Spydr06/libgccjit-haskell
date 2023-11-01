module GccJit.Utils (
    AsObject(..),
    AsType(..)
) where

#include <libgccjit.h>

import Foreign.Ptr

import GccJit.Foreign
import GccJit.Types

class AsObject a where
    asObject :: Ptr a -> IO (Ptr Object)

instance AsObject Object where
    asObject = return

instance AsObject Type where
    asObject = typeAsObject

instance AsObject RValue where
    asObject = rValueAsObject

instance AsObject LValue where
    asObject = lValueAsObject

instance AsObject Function where
    asObject = functionAsObject

instance AsObject Block where
    asObject = blockAsObject

instance AsObject Field where
    asObject = fieldAsObject

instance AsObject Param where
    asObject = paramAsObject

#ifdef LIBGCCJIT_HAVE_SWITCH_STATEMENTS
instance AsObject Case where
    asObject = caseAsObject
#endif

#ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS
instance AsObject ExtendedAsm where
    asObject = extendedAsmAsObject
#endif

class AsType a where
    asType :: Ptr a -> IO (Ptr Type)

instance AsType Type where
    asType = return

instance AsType Struct where
    asType = structAsType
