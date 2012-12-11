{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, DataKinds, TypeFamilies, TypeOperators #-}
module LLVM.Core.Vector(MkVector(..), vector, ) where
import Data.Function
import Data.Proxy
import LLVM.Core.Type
import LLVM.Core.Data
import LLVM.ExecutionEngine.Target
import Foreign.Ptr(castPtr)
import Foreign.Storable(Storable(..))
import Foreign.Marshal.Array(peekArray, pokeArray)
import System.IO.Unsafe(unsafePerformIO)
import GHC.TypeLits

-- XXX Should these really be here?
class ((1 <=? (VectorWidth va)) ~ 'True, IsPrimitive (VectorType va)) => MkVector va where
    type VectorType va :: *
    type VectorWidth va :: Nat
    toVector :: va -> Vector (VectorWidth va) (VectorType va)
    fromVector :: Vector (VectorWidth va) (VectorType va) -> va

{-
instance (IsPrimitive a) => MkVector (Value a) D1 (Value a) where
    toVector a = Vector [a]
-}

instance (IsPrimitive a) => MkVector (a, a) where
    type VectorType (a, a) = a
    type VectorWidth (a, a) = 2
    toVector (a1, a2) = Vector [a1, a2]
    fromVector (Vector [a1, a2]) = (a1, a2)
    fromVector _ = error "fromVector: impossible"

instance (IsPrimitive a) => MkVector (a, a, a, a) where
    type VectorType (a, a, a, a) = a
    type VectorWidth (a, a, a, a) = 4
    toVector (a1, a2, a3, a4) = Vector [a1, a2, a3, a4]
    fromVector (Vector [a1, a2, a3, a4]) = (a1, a2, a3, a4)
    fromVector _ = error "fromVector: impossible"

instance (IsPrimitive a) => MkVector (a, a, a, a, a, a, a, a) where
    type VectorType (a, a, a, a, a, a, a, a) = a
    type VectorWidth (a, a, a, a, a, a, a, a) = 8
    toVector (a1, a2, a3, a4, a5, a6, a7, a8) = Vector [a1, a2, a3, a4, a5, a6, a7, a8]
    fromVector (Vector [a1, a2, a3, a4, a5, a6, a7, a8]) = (a1, a2, a3, a4, a5, a6, a7, a8)
    fromVector _ = error "fromVector: impossible"

instance (Storable a, (1 <=? n) ~ 'True, SingI n, IsPrimitive a, IsType a) => Storable (Vector n a) where
    sizeOf _ = storeSizeOfType ourTargetData (typeRef (Proxy :: Proxy a))
    alignment _ = aBIAlignmentOfType ourTargetData (typeRef (Proxy :: Proxy a))
    peek p = fmap Vector $ peekArray (fromIntegral (fromSing (sing :: Sing n))) (castPtr p :: Ptr a)
    poke p (Vector vs) = pokeArray (castPtr p :: Ptr a) vs

-- XXX The JITer target data.  This isn't really right.
ourTargetData :: TargetData
ourTargetData = unsafePerformIO getTargetData

--------------------------------------

unVector :: Vector n a -> [a]
unVector (Vector xs) = xs

-- |Make a constant vector.  Replicates or truncates the list to get length /n/.
-- This behaviour is consistent with that of 'LLVM.Core.CodeGen.constVector'.
vector :: forall a n. ((1 <=? n) ~ 'True, SingI n) => [a] -> Vector n a
vector xs =
   Vector (take (fromIntegral (fromSing (sing :: Sing n))) (cycle xs))


binop :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
binop op xs ys = Vector $ zipWith op (unVector xs) (unVector ys)

unop :: (a -> b) -> Vector n a -> Vector n b
unop op = Vector . map op . unVector

instance (Eq a, (1 <=? n) ~ 'True, SingI n) => Eq (Vector n a) where
    (==) = (==) `on` unVector

instance (Ord a, (1 <=? n) ~ 'True, SingI n) => Ord (Vector n a) where
    compare = compare `on` unVector

instance (Num a, (1 <=? n) ~ 'True, SingI n) => Num (Vector n a) where
    (+) = binop (+)
    (-) = binop (-)
    (*) = binop (*)
    negate = unop negate
    abs = unop abs
    signum = unop signum
    fromInteger = Vector . replicate (fromIntegral (fromSing (sing :: Sing n))) . fromInteger

instance (Enum a, (1 <=? n) ~ 'True, SingI n) => Enum (Vector n a) where
    succ = unop succ
    pred = unop pred
    fromEnum = error "Vector fromEnum"
    toEnum = Vector . map toEnum . replicate (fromIntegral (fromSing (sing :: Sing n)))

instance (Real a, (1 <=? n) ~ 'True, SingI n) => Real (Vector n a) where
    toRational = error "Vector toRational"

instance (Integral a, (1 <=? n) ~ 'True, SingI n) => Integral (Vector n a) where
    quot = binop quot
    rem  = binop rem
    div  = binop div
    mod  = binop mod
    quotRem (Vector xs) (Vector ys) = (Vector qs, Vector rs) where (qs, rs) = unzip $ zipWith quotRem xs ys
    divMod  (Vector xs) (Vector ys) = (Vector qs, Vector rs) where (qs, rs) = unzip $ zipWith divMod  xs ys
    toInteger = error "Vector toInteger"

instance (Fractional a, (1 <=? n) ~ 'True, SingI n) => Fractional (Vector n a) where
    (/) = binop (/)
    fromRational = Vector . replicate (fromIntegral (fromSing (sing :: Sing n))) . fromRational

instance (RealFrac a, (1 <=? n) ~ 'True, SingI n) => RealFrac (Vector n a) where
    properFraction = error "Vector properFraction"

instance (Floating a, (1 <=? n) ~ 'True, SingI n) => Floating (Vector n a) where
    pi = Vector $ replicate (fromIntegral (fromSing (sing :: Sing n))) pi
    sqrt = unop sqrt
    log = unop log
    logBase = binop logBase
    (**) = binop (**)
    exp = unop exp
    sin = unop sin
    cos = unop cos
    tan = unop tan
    asin = unop asin
    acos = unop acos
    atan = unop atan
    sinh = unop sinh
    cosh = unop cosh
    tanh = unop tanh
    asinh = unop asinh
    acosh = unop acosh
    atanh = unop atanh

instance (RealFloat a, (1 <=? n) ~ 'True, SingI n) => RealFloat (Vector n a) where
    floatRadix = floatRadix . head . unVector
    floatDigits = floatDigits . head . unVector
    floatRange = floatRange . head . unVector
    decodeFloat = error "Vector decodeFloat"
    encodeFloat = error "Vector encodeFloat"
    exponent _ = 0
    scaleFloat 0 x = x
    scaleFloat _ _ = error "Vector scaleFloat"
    isNaN = error "Vector isNaN"
    isInfinite = error "Vector isInfinite"
    isDenormalized = error "Vector isDenormalized"
    isNegativeZero = error "Vector isNegativeZero"
    isIEEE = isIEEE . head . unVector
