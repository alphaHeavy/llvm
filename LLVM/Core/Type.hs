{-# LANGUAGE CPP, DeriveDataTypeable, EmptyDataDecls, FlexibleContexts,
  FlexibleInstances, FunctionalDependencies, IncoherentInstances,
  MultiParamTypeClasses, ScopedTypeVariables, TypeOperators,
  TypeSynonymInstances, UndecidableInstances, DataKinds, PolyKinds,
  TypeFamilies #-}
-- |The LLVM type system is captured with a number of Haskell type classes.
-- In general, an LLVM type @T@ is represented as @Value T@, where @T@ is some Haskell type.
-- The various types @T@ are classified by various type classes, e.g., 'IsFirstClass' for
-- those types that are LLVM first class types (passable as arguments etc).
-- All valid LLVM types belong to the 'IsType' class.
module LLVM.Core.Type(
    -- * Type classifier
    IsType(..),
    -- * StructFields classifier
    StructFields,
    -- ** Special type classifiers
    IsArithmetic(arithmeticType),
    ArithmeticType(IntegerType,FloatingType),
    IsInteger,
    IsIntegerOrPointer,
    IsFloating,
    IsPrimitive,
    IsFirstClass,
    IsFunction,
    SizeOf,
    -- ** Others
    NumberOfElements,
    UnknownSize, -- needed for arrays of structs
    -- ** Structs
    (:&), (&),
    -- ** Type tests
    TypeDesc(..),
    isFloating,
    isSigned,
    typeRef,
    typeName,
    typeDesc2,
    VarArgs, CastVarArgs,
    ) where
import Data.Typeable
import Data.List(intercalate)
import Data.Int
import Data.Word
import Data.Proxy
import Foreign.StablePtr (StablePtr, )
import LLVM.Core.Util(functionType, structType)
import LLVM.Core.Data
import qualified LLVM.FFI.Core as FFI
import GHC.TypeLits

#include "MachDeps.h"

-- TODO:
-- Move IntN, WordN to a special module that implements those types
--   properly in Haskell.
-- Also more Array and Vector to a Haskell module to implement them.
-- Add Label?
-- Add structures (using tuples, maybe nested).

-- |The 'IsType' class classifies all types that have an LLVM representation.
class IsType a where
    typeDesc :: Proxy a -> TypeDesc

typeRef :: forall a . (IsType a) => Proxy a -> FFI.TypeRef  -- ^The argument is never evaluated
typeRef = code . typeDesc
  where code TDFloat  = FFI.floatType
  	code TDDouble = FFI.doubleType
	code TDFP128  = FFI.fp128Type
	code TDVoid   = FFI.voidType
	code (TDInt _ n)  = FFI.integerType (fromInteger n)
	code (TDArray n a) = FFI.arrayType (code a) (fromInteger n)
	code (TDVector n a) = FFI.vectorType (code a) (fromInteger n)
	code (TDPtr a) = FFI.pointerType (code a) 0
	code (TDFunction va as b) = functionType va (code b) (map code as)
	code TDLabel = FFI.labelType
        code (TDStruct ts packed) = structType (map code ts) packed
        code TDInvalidType = error "typeRef TDInvalidType"

typeName :: (IsType a) => Proxy a -> String
typeName = code . typeDesc
  where code TDFloat  = "f32"
  	code TDDouble = "f64"
	code TDFP128  = "f128"
	code TDVoid   = "void"
	code (TDInt _ n)  = "i" ++ show n
	code (TDArray n a) = "[" ++ show n ++ " x " ++ code a ++ "]"
	code (TDVector n a) = "<" ++ show n ++ " x " ++ code a ++ ">"
	code (TDPtr a) = code a ++ "*"
	code (TDFunction _ as b) = code b ++ "(" ++ intercalate "," (map code as) ++ ")"
        code TDLabel = "label"
        code (TDStruct as packed) = (if packed then "<{" else "{") ++
                                    intercalate "," (map code as) ++
                                    (if packed then "}>" else "}")
        code TDInvalidType = error "typeName TDInvalidType"

typeDesc2 :: FFI.TypeRef -> IO TypeDesc
typeDesc2 t = do
    tk <- FFI.getTypeKind t
    case tk of
      FFI.VoidTypeKind -> return TDVoid
      FFI.FloatTypeKind -> return TDFloat
      FFI.DoubleTypeKind -> return TDDouble
      -- FIXME: FFI.X86_FP80TypeKind -> return "X86_FP80"
      FFI.FP128TypeKind -> return TDFP128
      -- FIXME: FFI.PPC_FP128TypeKind -> return "PPC_FP128"
      FFI.LabelTypeKind -> return TDLabel
      FFI.IntegerTypeKind -> do
                n <- FFI.getIntTypeWidth t
                return $ TDInt False (fromIntegral n)
      -- FIXME: FFI.FunctionTypeKind
      -- FIXME: FFI.StructTypeKind -> return "(Struct ...)"
      FFI.ArrayTypeKind -> do
                n <- FFI.getArrayLength t
                et <- FFI.getElementType t
                etd <- typeDesc2 et
                return $ TDArray (fromIntegral n) etd
      FFI.PointerTypeKind -> do
                et <- FFI.getElementType t
                etd <- typeDesc2 et
                return $ TDPtr etd
      -- FIXME: FFI.OpaqueTypeKind -> return "Opaque"
      FFI.VectorTypeKind -> do
                n <- FFI.getVectorSize t
                et <- FFI.getElementType t
                etd <- typeDesc2 et
                return $ TDVector (fromIntegral n) etd
      -- FIXME: LLVMMetadataTypeKind,    /**< Metadata */
      -- FIXME: LLVMX86_MMXTypeKind      /**< X86 MMX */
      _ -> return TDInvalidType

-- |Type descriptor, used to convey type information through the LLVM API.
data TypeDesc = TDFloat | TDDouble | TDFP128 | TDVoid | TDInt Bool Integer
              | TDArray Integer TypeDesc | TDVector Integer TypeDesc
	      | TDPtr TypeDesc | TDFunction Bool [TypeDesc] TypeDesc | TDLabel
              | TDStruct [TypeDesc] Bool | TDInvalidType
    deriving (Eq, Ord, Show, Typeable)

-- XXX isFloating and typeName could be extracted from typeRef
-- Usage:
--   superclass of IsConst
--   add, sub, mul, neg context
--   used to get type name to call intrinsic
-- |Arithmetic types, i.e., integral and floating types.
class IsFirstClass a => IsArithmetic a where
    arithmeticType :: ArithmeticType a

data ArithmeticType a = IntegerType | FloatingType

instance Functor ArithmeticType where
    fmap _ IntegerType  = IntegerType
    fmap _ FloatingType = FloatingType

-- Usage:
--  constI, allOnes
--  many instructions.  XXX some need vector
--  used to find signedness in Arithmetic
-- |Integral types.
class (IsArithmetic a, IsIntegerOrPointer a) => IsInteger a

-- Usage:
--  icmp
-- |Integral or pointer type.
class IsIntegerOrPointer a

isSigned :: forall a . (IsInteger a) => Proxy a -> Bool
isSigned = is . typeDesc
  where is (TDInt s _) = s
  	is (TDVector _ a) = is a
	is _ = error "isSigned got impossible input"

-- Usage:
--  constF
--  many instructions
-- |Floating types.
class IsArithmetic a => IsFloating a

isFloating :: (IsArithmetic a) => Proxy a -> Bool
isFloating = is . typeDesc
  where is TDFloat = True
  	is TDDouble = True
	is TDFP128 = True
	is (TDVector _ a) = is a
	is _ = False

-- Usage:
--  Precondition for Vector
-- |Primitive types.
class (NumberOfElements a ~ 1) => IsPrimitive a

-- |Number of elements for instructions that handle both primitive and vector types
type family NumberOfElements a :: Nat


-- Usage:
--  Precondition for function args and result.
--  Used by some instructions, like ret and phi.
--  XXX IsSized as precondition?
-- |First class types, i.e., the types that can be passed as arguments, etc.
class IsType a => IsFirstClass a

-- Usage:
--  Context for Array being a type
--  thus, allocation instructions
-- |Types with a fixed size.
type family SizeOf a :: Nat

-- |Function type.
class (IsType a) => IsFunction a where
    funcType :: [TypeDesc] -> Proxy a -> TypeDesc

-- Only make instances for types that make sense in Haskell
-- (i.e., some floating types are excluded).

-- Floating point types.
instance IsType Float  where typeDesc _ = TDFloat
instance IsType Double where typeDesc _ = TDDouble
instance IsType FP128  where typeDesc _ = TDFP128

-- Void type
instance IsType ()     where typeDesc _ = TDVoid

-- Label type
instance IsType Label  where typeDesc _ = TDLabel

-- Variable size integer types
instance ((1 <=? n) ~ 'True, SingI n) => IsType (IntN n)
    where typeDesc _ = TDInt True  (fromSing (sing :: Sing n))

instance ((1 <=? n) ~ 'True, SingI n) => IsType (WordN n)
    where typeDesc _ = TDInt False (fromSing (sing :: Sing n))

-- Fixed size integer types.
instance IsType Bool   where typeDesc _ = TDInt False  1
instance IsType Word8  where typeDesc _ = TDInt False  8
instance IsType Word16 where typeDesc _ = TDInt False 16
instance IsType Word32 where typeDesc _ = TDInt False 32
instance IsType Word64 where typeDesc _ = TDInt False 64
instance IsType Int8   where typeDesc _ = TDInt True   8
instance IsType Int16  where typeDesc _ = TDInt True  16
instance IsType Int32  where typeDesc _ = TDInt True  32
instance IsType Int64  where typeDesc _ = TDInt True  64

-- Sequence types
instance (SingI n, IsType a) => IsType (Array n a)
    where typeDesc _ = TDArray (fromSing (sing :: Sing n))
    	  	               (typeDesc (Proxy :: Proxy a))
instance ((1 <=? n) ~ 'True, IsPrimitive a, IsType a, SingI n) => IsType (Vector n a)
    where typeDesc _ = TDVector (fromSing (sing :: Sing n))
    	  	       		(typeDesc (Proxy :: Proxy a))

-- Pointer type.
instance (IsType a) => IsType (Ptr a) where
    typeDesc _ = TDPtr (typeDesc (Proxy :: Proxy a))

instance IsType (StablePtr a) where
    typeDesc _ = TDPtr (typeDesc (Proxy :: Proxy Int8))
{-
    typeDesc _ = TDPtr TDVoid

List: Type.cpp:1311: static llvm::PointerType* llvm::PointerType::get(const llvm::Type*, unsigned int): Assertion `ValueType != Type::VoidTy && "Pointer to void is not valid, use sbyte* instead!"' failed.
-}


-- Functions.
instance (IsFirstClass a, IsFunction b) => IsType (a->b) where
    typeDesc = funcType []

-- Function base type, always IO.
instance (IsFirstClass a) => IsType (IO a) where
    typeDesc = funcType []

-- Struct types, basically a list of component types.
instance (StructFields a) => IsType (Struct a) where
    typeDesc _ = TDStruct (fieldTypes (Proxy :: Proxy a)) False

instance (StructFields a) => IsType (PackedStruct a) where
    typeDesc _ = TDStruct (fieldTypes (Proxy :: Proxy a)) True

-- Use a nested tuples for struct fields.
class StructFields (as :: [*]) where
    fieldTypes :: Proxy as -> [TypeDesc]

instance (SizeOf a ~ sa, StructFields as, IsType a) => StructFields (a ': as) where
    fieldTypes _ = typeDesc (Proxy :: Proxy a) : fieldTypes (Proxy :: Proxy as)
instance StructFields '[] where
    fieldTypes _ = []

-- An alias for pairs to make structs look nicer
infixr :&
type (:&) a as = (a, as)
infixr &
(&) :: a -> as -> a :& as
a & as = (a, as)

--- Instances to classify types
instance IsArithmetic Float  where arithmeticType = FloatingType
instance IsArithmetic Double where arithmeticType = FloatingType
instance IsArithmetic FP128  where arithmeticType = FloatingType
instance ((1 <=? n) ~ 'True, SingI n) => IsArithmetic (IntN n)  where arithmeticType = IntegerType
instance ((1 <=? n) ~ 'True, SingI n) => IsArithmetic (WordN n) where arithmeticType = IntegerType
instance IsArithmetic Bool   where arithmeticType = IntegerType
instance IsArithmetic Int8   where arithmeticType = IntegerType
instance IsArithmetic Int16  where arithmeticType = IntegerType
instance IsArithmetic Int32  where arithmeticType = IntegerType
instance IsArithmetic Int64  where arithmeticType = IntegerType
instance IsArithmetic Word8  where arithmeticType = IntegerType
instance IsArithmetic Word16 where arithmeticType = IntegerType
instance IsArithmetic Word32 where arithmeticType = IntegerType
instance IsArithmetic Word64 where arithmeticType = IntegerType
instance ((1 <=? n) ~ 'True, IsPrimitive a, IsArithmetic a, SingI n) =>
         IsArithmetic (Vector n a) where
   arithmeticType = fmap (undefined :: a -> Vector n a) arithmeticType

instance IsFloating Float
instance IsFloating Double
instance IsFloating FP128
instance ((1 <=? n) ~ 'True, IsPrimitive a, IsFloating a, SingI n) => IsFloating (Vector n a)

instance ((1 <=? n) ~ 'True, SingI n) => IsInteger (IntN n)
instance ((1 <=? n) ~ 'True, SingI n) => IsInteger (WordN n)
instance IsInteger Bool
instance IsInteger Int8
instance IsInteger Int16
instance IsInteger Int32
instance IsInteger Int64
instance IsInteger Word8
instance IsInteger Word16
instance IsInteger Word32
instance IsInteger Word64
instance ((1 <=? n) ~ 'True, IsPrimitive a, IsInteger a, SingI n) => IsInteger (Vector n a)

instance ((1 <=? n) ~ 'True, SingI n) => IsIntegerOrPointer (IntN n)
instance ((1 <=? n) ~ 'True, SingI n) => IsIntegerOrPointer (WordN n)
instance IsIntegerOrPointer Bool
instance IsIntegerOrPointer Int8
instance IsIntegerOrPointer Int16
instance IsIntegerOrPointer Int32
instance IsIntegerOrPointer Int64
instance IsIntegerOrPointer Word8
instance IsIntegerOrPointer Word16
instance IsIntegerOrPointer Word32
instance IsIntegerOrPointer Word64
instance ((1 <=? n) ~ 'True, IsPrimitive a, IsInteger a, SingI n) => IsIntegerOrPointer (Vector n a)
instance (IsType a) => IsIntegerOrPointer (Ptr a)

instance IsFirstClass Float
instance IsFirstClass Double
instance IsFirstClass FP128
instance ((1 <=? n) ~ 'True, SingI n) => IsFirstClass (IntN n)
instance ((1 <=? n) ~ 'True, SingI n) => IsFirstClass (WordN n)
instance IsFirstClass Bool
instance IsFirstClass Int8
instance IsFirstClass Int16
instance IsFirstClass Int32
instance IsFirstClass Int64
instance IsFirstClass Word8
instance IsFirstClass Word16
instance IsFirstClass Word32
instance IsFirstClass Word64
instance ((1 <=? n) ~ 'True, IsPrimitive a, IsType a, SingI n) => IsFirstClass (Vector n a)
instance (IsType a, SingI n) => IsFirstClass (Array n a)
instance (IsType a) => IsFirstClass (Ptr a)
instance IsFirstClass (StablePtr a)
instance IsFirstClass Label
instance IsFirstClass () -- XXX This isn't right, but () can be returned
instance (StructFields as) => IsFirstClass (Struct as)

type instance SizeOf Float = 32
type instance SizeOf Double = 64
type instance SizeOf FP128 = 128
type instance SizeOf (IntN n) = n
type instance SizeOf (WordN n) = n
type instance SizeOf Bool = 1
type instance SizeOf Int8 = 8
type instance SizeOf Int16 = 16
type instance SizeOf Int32 = 32
type instance SizeOf Int64 = 64
type instance SizeOf Word8 = 8
type instance SizeOf Word16 = 16
type instance SizeOf Word32 = 32
type instance SizeOf Word64 = 64
type instance SizeOf (Array n a) = SizeOf a * n
type instance SizeOf (Vector n a) = SizeOf a * n
type instance SizeOf (Ptr a) = PtrSize
type instance SizeOf (StablePtr a) = PtrSize
-- instance SizeOf Label PtrSize -- labels are not quite first classed
-- We cannot compute the sizes statically :(
type instance SizeOf (Struct as) = UnknownSize
type instance SizeOf (PackedStruct as) = UnknownSize

type UnknownSize = 99   -- XXX this is wrong!

#if WORD_SIZE_IN_BITS == 32
type PtrSize = 32
#elif WORD_SIZE_IN_BITS == 64
type PtrSize = 64
#else
#error cannot determine type of PtrSize
#endif

instance IsPrimitive Float
instance IsPrimitive Double
instance IsPrimitive FP128
instance IsPrimitive (IntN n)
instance IsPrimitive (WordN n)
instance IsPrimitive Bool
instance IsPrimitive Int8
instance IsPrimitive Int16
instance IsPrimitive Int32
instance IsPrimitive Int64
instance IsPrimitive Word8
instance IsPrimitive Word16
instance IsPrimitive Word32
instance IsPrimitive Word64
instance IsPrimitive Label
instance IsPrimitive ()


type instance NumberOfElements Float = 1
type instance NumberOfElements Double = 1
type instance NumberOfElements FP128 = 1
type instance NumberOfElements (IntN n) = 1
type instance NumberOfElements (WordN n) = 1
type instance NumberOfElements Bool = 1
type instance NumberOfElements Int8 = 1
type instance NumberOfElements Int16 = 1
type instance NumberOfElements Int32 = 1
type instance NumberOfElements Int64 = 1
type instance NumberOfElements Word8 = 1
type instance NumberOfElements Word16 = 1
type instance NumberOfElements Word32 = 1
type instance NumberOfElements Word64 = 1
type instance NumberOfElements Label = 1
type instance NumberOfElements () = 1
type instance NumberOfElements (Vector n a) = n

-- Functions.
instance (IsFirstClass a, IsFunction b) => IsFunction (a -> b) where
    funcType ts _ = funcType (typeDesc (Proxy :: Proxy a) : ts) (Proxy :: Proxy b)
instance (IsFirstClass a) => IsFunction (IO a) where
    funcType ts _ = TDFunction False (reverse ts) (typeDesc (Proxy :: Proxy a))
instance (IsFirstClass a) => IsFunction (VarArgs a) where
    funcType ts _ = TDFunction True  (reverse ts) (typeDesc (Proxy :: Proxy a))

-- |The 'VarArgs' type is a placeholder for the real 'IO' type that
-- can be obtained with 'castVarArgs'.
data VarArgs (a :: *)
    deriving (Typeable)
instance IsType (VarArgs a) where
    typeDesc _ = error "typeDesc: Dummy type VarArgs used incorrectly"

-- |Define what vararg types are permissible.
class CastVarArgs a b
instance (CastVarArgs b c) => CastVarArgs (a -> b) (a -> c)
instance CastVarArgs (VarArgs a) (IO a)
instance (IsFirstClass a, CastVarArgs (VarArgs b) c) => CastVarArgs (VarArgs b) (a -> c)




-- XXX Structures not implemented.  Tuples is probably an easy way.

