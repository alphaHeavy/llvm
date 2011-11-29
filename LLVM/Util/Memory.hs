{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.Util.Memory where
{-
  ( memcpy
  , memmove
  , memset
  , IsLengthType
  ) where

import Control.Monad.Trans
import LLVM.Core
import Data.Word (Word8, Word32, Word64)

class IsFirstClass len => IsLengthType len where

instance IsLengthType Word32 where
instance IsLengthType Word64 where


memcpyFunc :: forall len m . (MonadIO m, IsLengthType len)
           => CodeGenModule m (Ptr Word8 -> Ptr Word8 -> len -> Word32 -> Bool -> IO ())
memcpyFunc = externFunction $ "llvm.memcpy.p0i8.p0i8." ++ typeName (undefined :: len)

memcpy :: (Functor m, MonadIO m,  IsLengthType len)
       => CodeGenModule m (Value (Ptr Word8) -> Value (Ptr Word8) -> Value len -> Value Word32 -> Value Bool -> CodeGenFunction r m ())
memcpy = fmap (\f dest src len align volatile -> fmap (const()) $ call f dest src len align volatile) memcpyFunc

memmoveFunc :: forall len m .  IsLengthType len
            => TFunction m (Ptr Word8 -> Ptr Word8 -> len -> Word32 -> Bool -> IO ())
memmoveFunc = externFunction $ "llvm.memmove.p0i8.p0i8." ++ typeName (undefined :: len)

memmove :: (Functor m, MonadIO m, IsLengthType len)
        => CodeGenModule m (Value (Ptr Word8) -> Value (Ptr Word8) -> Value len -> Value Word32 -> Value Bool -> CodeGenFunction r m ())
memmove = fmap (\f dest src len align volatile -> fmap (const()) $ call f dest src len align volatile) memmoveFunc

memsetFunc :: forall len m . (MonadIO m, IsLengthType len)
           => TFunction m (Ptr Word8 -> Word8 -> len -> Word32 -> Bool -> IO ())
memsetFunc =
   newNamedFunction ExternalLinkage $
      "llvm.memset.p0i8." ++ typeName (undefined :: len)

memset :: (Functor m, MonadIO m, IsLengthType len)
       => CodeGenModule m (Value (Ptr Word8) -> Value Word8 -> Value len -> Value Word32 -> Value Bool -> CodeGenFunction r m ())
memset = fmap (\f dest val len align volatile -> fmap (const()) $ call f dest val len align volatile) memsetFunc
-}

