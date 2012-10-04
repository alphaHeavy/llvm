{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, KindSignatures, DataKinds #-}
module LLVM.Core.Data(IntN(..), WordN(..), FP128(..),
       		      Array(..), Vector(..), Ptr, Label, Struct(..), PackedStruct(..)) where
import Data.Typeable
import Foreign.Ptr(Ptr)
import GHC.TypeLits

-- TODO:
-- Make instances IntN, WordN to actually do the right thing.
-- Make FP128 do the right thing
-- Make Array functions.

-- |Variable sized signed integer.
-- The /n/ parameter should belong to @PosI@.
newtype IntN (n :: Nat) = IntN Integer
    deriving (Show)

-- |Variable sized unsigned integer.
-- The /n/ parameter should belong to @PosI@.
newtype WordN (n :: Nat) = WordN Integer
    deriving (Show)

-- |128 bit floating point.
newtype FP128 = FP128 Rational
    deriving (Show)

-- |Fixed sized arrays, the array size is encoded in the /n/ parameter.
newtype Array (n :: Nat) a = Array [a]
    deriving (Show)

-- |Fixed sized vector, the array size is encoded in the /n/ parameter.
newtype Vector (n :: Nat) a = Vector [a]
    deriving (Show)

-- |Label type, produced by a basic block.
data Label
    deriving (Typeable)

-- |Struct types; a list (nested tuple) of component types.
data Struct (a :: [*]) = Struct
    deriving (Show)

data PackedStruct (a :: [*]) = PackedStruct
    deriving (Show)
