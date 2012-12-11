{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances, FlexibleContexts, TypeOperators, DeriveDataTypeable, ForeignFunctionInterface, DataKinds, PolyKinds, TypeFamilies #-}
module LLVM.Core.Instructions(
    -- * ADT representation of IR
    BinOpDesc(..), InstrDesc(..), ArgDesc(..), getInstrDesc,
    -- * Terminator instructions
    ret,
    condBr,
    br,
    switch,
    invoke,
    -- Removed in LLVM_3.0
    -- unwind,
    unreachable,
    -- * Arithmetic binary operations
    -- | Arithmetic operations with the normal semantics.
    -- The u instractions are unsigned, the s instructions are signed.
    add, sub, mul, neg,
    iadd, isub, imul, ineg,
    fadd, fsub, fmul, fneg,
    idiv, irem,
    udiv, sdiv, fdiv, urem, srem, frem,
    -- * Logical binary operations
    -- |Logical instructions with the normal semantics.
    shl, lshr, ashr, and, or, xor, inv,
    -- * Vector operations
    extractelement,
    insertelement,
    shufflevector,
    -- * Aggregate operation
    extractvalue,
    insertvalue,
    -- * Memory access
    malloc, arrayMalloc,
    alloca, arrayAlloca,
    free,
    load,
    store,
    getElementPtr, getElementPtr0,
    -- * Conversions
    trunc, zext, sext,
    fptrunc, fpext,
    fptoui, fptosi, fptoint,
    uitofp, sitofp, inttofp,
    ptrtoint, inttoptr,
    bitcast, bitcastUnify,
    -- * Comparison
    CmpPredicate(..), IntPredicate(..), FPPredicate(..),
    CmpOp, CmpOpType, CmpOpRetType,
    CmpRet, CmpRetType,
    cmp, pcmp, icmp, fcmp,
    select,
    -- * Other
    phi, addPhiInputs,
    call,

    -- * Classes and types
    Terminate,
    Ret, CallArgs, ABinOp, FunctionArgs, IsConst,
    AllocArg,
    GetElementPtr, GetElementPtrType, IsIndexArg, GetValue
    ) where
import Prelude hiding (and, or)
import Data.Typeable
import Control.Monad(liftM)
import Data.Int
import Data.Word
import Data.Map(fromList, (!))
import Data.Proxy
import Foreign.Ptr (FunPtr, )
import Foreign.C(CInt, CUInt)
import qualified LLVM.FFI.Core as FFI
import LLVM.Core.Data
import LLVM.Core.Type
import LLVM.Core.CodeGenMonad
import LLVM.Core.CodeGen
import qualified LLVM.Core.Util as U
import GHC.TypeLits

-- TODO:
-- Add vector version of arithmetic
-- Add rest of instructions
-- Use Terminate to ensure bb termination (how?)
-- more intrinsics are needed to, e.g., create an empty vector

data ArgDesc = AV String | AI Int | AL String | AE

instance Show ArgDesc where
    -- show (AV s) = "V_" ++ s
    -- show (AI i) = "I_" ++ show i
    -- show (AL l) = "L_" ++ l
    show (AV s) = s
    show (AI i) = show i
    show (AL l) = l
    show AE = "voidarg?"

data BinOpDesc = BOAdd | BOAddNuw | BOAddNsw | BOAddNuwNsw | BOFAdd
               | BOSub | BOSubNuw | BOSubNsw | BOSubNuwNsw | BOFSub
               | BOMul | BOMulNuw | BOMulNsw | BOMulNuwNsw | BOFMul
               | BOUDiv | BOSDiv | BOSDivExact | BOFDiv | BOURem | BOSRem | BOFRem
               | BOShL | BOLShR | BOAShR | BOAnd | BOOr | BOXor
    deriving Show

-- FIXME: complete definitions for unimplemented instructions
data InstrDesc =
    -- terminators
    IDRet TypeDesc ArgDesc | IDRetVoid
  | IDBrCond ArgDesc ArgDesc ArgDesc | IDBrUncond ArgDesc
  | IDSwitch [(ArgDesc, ArgDesc)]
  | IDIndirectBr
  | IDInvoke
  | IDUnwind
  | IDUnreachable
    -- binary operators (including bitwise)
  | IDBinOp BinOpDesc TypeDesc ArgDesc ArgDesc
    -- memory access and addressing
  | IDAlloca TypeDesc Int Int | IDLoad TypeDesc ArgDesc | IDStore TypeDesc ArgDesc ArgDesc
  | IDGetElementPtr TypeDesc [ArgDesc]
    -- conversion
  | IDTrunc TypeDesc TypeDesc ArgDesc | IDZExt TypeDesc TypeDesc ArgDesc
  | IDSExt TypeDesc TypeDesc ArgDesc | IDFPtoUI TypeDesc TypeDesc ArgDesc
  | IDFPtoSI TypeDesc TypeDesc ArgDesc | IDUItoFP TypeDesc TypeDesc ArgDesc
  | IDSItoFP TypeDesc TypeDesc ArgDesc
  | IDFPTrunc TypeDesc TypeDesc ArgDesc | IDFPExt TypeDesc TypeDesc ArgDesc
  | IDPtrToInt TypeDesc TypeDesc ArgDesc | IDIntToPtr TypeDesc TypeDesc ArgDesc
  | IDBitcast TypeDesc TypeDesc ArgDesc
    -- other
  | IDICmp IntPredicate ArgDesc ArgDesc | IDFCmp FPPredicate ArgDesc ArgDesc
  | IDPhi TypeDesc [(ArgDesc, ArgDesc)] | IDCall TypeDesc ArgDesc [ArgDesc]
  | IDSelect TypeDesc ArgDesc ArgDesc | IDUserOp1 | IDUserOp2 | IDVAArg
    -- vector operators
  | IDExtractElement | IDInsertElement | IDShuffleVector
    -- aggregate operators
  | IDExtractValue | IDInsertValue
    -- invalid
  | IDInvalidOp
    deriving Show

-- TODO: overflow support for binary operations (add/sub/mul)
getInstrDesc :: FFI.ValueRef -> IO (String, InstrDesc)
getInstrDesc v = do
    valueName <- U.getValueNameU v
    opcode <- FFI.instGetOpcode v
    t <- FFI.typeOf v >>= typeDesc2
    -- FIXME: sizeof() does not work for types!
    --tsize <- FFI.typeOf v -- >>= FFI.sizeOf -- >>= FFI.constIntGetZExtValue >>= return . fromIntegral
    tsize <- return 1
    os <- U.getOperands v >>= mapM getArgDesc
    os0 <- if length os > 0 then return $ os !! 0 else return AE
    os1 <- if length os > 1 then return $ os !! 1 else return AE
    t2 <- (if not (null os) && (opcode >= 30 || opcode <= 41)
            then U.getOperands v >>= return . snd . head >>= FFI.typeOf >>= typeDesc2
            else return TDVoid)
    p <- if opcode `elem` [42, 43] then FFI.cmpInstGetPredicate v else return 0
    let instr =
            (if opcode >= 8 && opcode <= 25 -- binary arithmetic
             then IDBinOp (getBinOp opcode) t os0 os1
             else if opcode >= 30 && opcode <= 41 -- conversion
                  then (getConvOp opcode) t2 t os0
                  else case opcode of
                         { 1 -> if null os then IDRetVoid else IDRet t os0;
                           2 -> if length os == 1 then IDBrUncond os0 else IDBrCond os0 (os !! 2) os1;
                           3 -> IDSwitch $ toPairs os;
                           -- TODO (can skip for now)
                           -- 4 -> IndirectBr ; 5 -> Invoke ;
                           6 -> IDUnwind; 7 -> IDUnreachable;
                           26 -> IDAlloca (getPtrType t) tsize (getImmInt os0);
                           27 -> IDLoad t os0; 28 -> IDStore t os0 os1;
                           29 -> IDGetElementPtr t os;
                           42 -> IDICmp (toIntPredicate p) os0 os1;
                           43 -> IDFCmp (toFPPredicate p) os0 os1;
                           44 -> IDPhi t $ toPairs os;
                           -- FIXME: getelementptr arguments are not handled
                           45 -> IDCall t (last os) (init os);
                           46 -> IDSelect t os0 os1;
                           -- TODO (can skip for now)
                           -- 47 -> UserOp1 ; 48 -> UserOp2 ; 49 -> VAArg ;
                           -- 50 -> ExtractElement ; 51 -> InsertElement ; 52 -> ShuffleVector ;
                           -- 53 -> ExtractValue ; 54 -> InsertValue ;
                           _ -> IDInvalidOp })
    return (valueName, instr)
    --if instr /= InvalidOp then return instr else fail $ "Invalid opcode: " ++ show opcode
        where getBinOp o = fromList [(8, BOAdd), (9, BOFAdd), (10, BOSub), (11, BOFSub),
                                     (12, BOMul), (13, BOFMul), (14, BOUDiv), (15, BOSDiv),
                                     (16, BOFDiv), (17, BOURem), (18, BOSRem), (19, BOFRem),
                                     (20, BOShL), (21, BOLShR), (22, BOAShR), (23, BOAnd),
                                     (24, BOOr), (25, BOXor)] ! o
              getConvOp o = fromList [(30, IDTrunc), (31, IDZExt), (32, IDSExt), (33, IDFPtoUI),
                                      (34, IDFPtoSI), (35, IDUItoFP), (36, IDSItoFP), (37, IDFPTrunc),
                                      (38, IDFPExt), (39, IDPtrToInt), (40, IDIntToPtr), (41, IDBitcast)] ! o
              toPairs xs = zip (stride 2 xs) (stride 2 (drop 1 xs))
              stride _ [] = []
              stride n (x:xs) = x : stride n (drop (n-1) xs)
              getPtrType (TDPtr t) = t
              getPtrType _ = TDVoid
              getImmInt (AI i) = i
              getImmInt _ = 0

-- TODO: fix for non-int constants
getArgDesc :: (String, FFI.ValueRef) -> IO ArgDesc
getArgDesc (vname, v) = do
    isC <- U.isConstant v
    t <- FFI.typeOf v >>= typeDesc2
    if isC
      then case t of
             TDInt _ _ -> do
                          cV <- FFI.constIntGetSExtValue v
                          return $ AI $ fromIntegral cV
             _ -> return AE
      else case t of
             TDLabel -> return $ AL vname
             _ -> return $ AV vname

--------------------------------------

type Terminate = Result ()

terminate :: Terminate
terminate = Result

--------------------------------------

-- |Acceptable arguments to the 'ret' instruction.
-- | Return from the current function with the given value.  Use () as the return value for what would be a void function is C.
class Ret a where
    type RetType a
    ret :: a -> CodeGenFunction (Result (RetType a))

instance Ret (ConstValue a) where
    type RetType (ConstValue a) = a
    ret (Value a) = do
        withCurrentBuilder_ $ \ bldPtr -> FFI.buildRet bldPtr a
        return (Result :: Result a)

instance Ret (Value a) where
    type RetType (Value a) = a
    ret (Value a) = do
        withCurrentBuilder_ $ \ bldPtr -> FFI.buildRet bldPtr a
        return (Result :: Result a)

instance Ret () where
    type RetType () = ()
    ret _ = do
        withCurrentBuilder_ $ FFI.buildRetVoid
        return terminate

withCurrentBuilder_ :: (FFI.BuilderRef -> IO a) -> CodeGenFunction ()
withCurrentBuilder_ p = withCurrentBuilder p >> return ()

--------------------------------------

-- | Branch to the first basic block if the boolean is true, otherwise to the second basic block.
condBr :: Value Bool -- ^ Boolean to branch upon.
       -> BasicBlock -- ^ Target for true.
       -> BasicBlock -- ^ Target for false.
       -> CodeGenFunction Terminate
condBr (Value b) (BasicBlock t1) (BasicBlock t2) = do
    withCurrentBuilder_ $ \ bldPtr -> FFI.buildCondBr bldPtr b t1 t2
    return terminate

--------------------------------------

-- | Unconditionally branch to the given basic block.
br :: BasicBlock  -- ^ Branch target.
   -> CodeGenFunction Terminate
br (BasicBlock t) = do
    withCurrentBuilder_ $ \ bldPtr -> FFI.buildBr bldPtr t
    return terminate

--------------------------------------

-- | Branch table instruction.
switch :: (IsInteger a)
       => Value a                        -- ^ Value to branch upon.
       -> BasicBlock                     -- ^ Default branch target.
       -> [(ConstValue a, BasicBlock)]   -- ^ Labels and corresponding branch targets.
       -> CodeGenFunction Terminate
switch (Value val) (BasicBlock dflt) arms = do
    withCurrentBuilder_ $ \ bldPtr -> do
        inst <- FFI.buildSwitch bldPtr val dflt (fromIntegral $ length arms)
        sequence_ [ FFI.addCase inst c b | (Value c, BasicBlock b) <- arms ]
    return terminate

--------------------------------------

-- Removed in LLVM_3.0
-- |Unwind the call stack until a function call performed with 'invoke' is reached.
-- I.e., throw a non-local exception.
-- unwind :: CodeGenFunction r Terminate
-- unwind = do
--     withCurrentBuilder_ FFI.buildUnwind
--     return terminate

-- |Inform the code generator that this code can never be reached.
unreachable :: CodeGenFunction Terminate
unreachable = do
    withCurrentBuilder_ FFI.buildUnreachable
    return terminate

--------------------------------------

type FFIBinOp = FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef
type FFIConstBinOp = FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef


withArithmeticType ::
    (IsArithmetic c) =>
    (ArithmeticType c -> a -> m (v c)) ->
    (a -> m (v c))
withArithmeticType f = f arithmeticType

-- |Acceptable arguments to arithmetic binary instructions.
class ABinOp a b c | a b -> c where
    abinop :: FFIConstBinOp -> FFIBinOp -> a -> b -> CodeGenFunction c

add :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
add =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> abinop FFI.constAdd  FFI.buildAdd
      FloatingType -> abinop FFI.constFAdd FFI.buildFAdd

sub :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
sub =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> abinop FFI.constSub  FFI.buildSub
      FloatingType -> abinop FFI.constFSub FFI.buildFSub

mul :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
mul =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> abinop FFI.constMul  FFI.buildMul
      FloatingType -> abinop FFI.constFMul FFI.buildFMul

iadd :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
iadd = abinop FFI.constAdd FFI.buildAdd
isub :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
isub = abinop FFI.constSub FFI.buildSub
imul :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
imul = abinop FFI.constMul FFI.buildMul

-- | signed or unsigned integer division depending on the type
idiv ::
   forall a b c v. (IsInteger c, ABinOp a b (v c)) =>
   a -> b -> CodeGenFunction (v c)
idiv =
   if isSigned (Proxy :: Proxy c)
     then abinop FFI.constSDiv FFI.buildSDiv
     else abinop FFI.constUDiv FFI.buildUDiv
-- | signed or unsigned remainder depending on the type
irem ::
   forall a b c v. (IsInteger c, ABinOp a b (v c)) =>
   a -> b -> CodeGenFunction (v c)
irem =
   if isSigned (Proxy :: Proxy c)
     then abinop FFI.constSRem FFI.buildSRem
     else abinop FFI.constURem FFI.buildURem

{-# DEPRECATED udiv "use idiv instead" #-}
{-# DEPRECATED sdiv "use idiv instead" #-}
{-# DEPRECATED urem "use irem instead" #-}
{-# DEPRECATED srem "use irem instead" #-}
udiv :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
udiv = abinop FFI.constUDiv FFI.buildUDiv
sdiv :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
sdiv = abinop FFI.constSDiv FFI.buildSDiv
urem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
urem = abinop FFI.constURem FFI.buildURem
srem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
srem = abinop FFI.constSRem FFI.buildSRem

fadd :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
fadd = abinop FFI.constFAdd FFI.buildFAdd
fsub :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
fsub = abinop FFI.constFSub FFI.buildFSub
fmul :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
fmul = abinop FFI.constFMul FFI.buildFMul

-- | Floating point division.
fdiv :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
fdiv = abinop FFI.constFDiv FFI.buildFDiv
-- | Floating point remainder.
frem :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
frem = abinop FFI.constFRem FFI.buildFRem

shl :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
shl  = abinop FFI.constShl  FFI.buildShl
lshr :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
lshr = abinop FFI.constLShr FFI.buildLShr
ashr :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
ashr = abinop FFI.constAShr FFI.buildAShr
and :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
and  = abinop FFI.constAnd  FFI.buildAnd
or :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
or   = abinop FFI.constOr   FFI.buildOr
xor :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction (v c)
xor  = abinop FFI.constXor  FFI.buildXor

instance ABinOp (Value a) (Value a) (Value a) where
    abinop _ op (Value a1) (Value a2) = buildBinOp op a1 a2

instance ABinOp (ConstValue a) (Value a) (Value a) where
    abinop _ op (Value a1) (Value a2) = buildBinOp op a1 a2

instance ABinOp (Value a) (ConstValue a) (Value a) where
    abinop _ op (Value a1) (Value a2) = buildBinOp op a1 a2

instance ABinOp (ConstValue a) (ConstValue a) (ConstValue a) where
    abinop cop _ (Value a1) (Value a2) =
        return $ Value $ cop a1 a2

instance (IsConst a) => ABinOp (Value a) a (Value a) where
    abinop cop op a1 a2 = abinop cop op a1 (constOf a2)

instance (IsConst a) => ABinOp a (Value a) (Value a) where
    abinop cop op a1 a2 = abinop cop op (constOf a1) a2

--instance (IsConst a) => ABinOp a a (ConstValue a) where
--    abinop cop op a1 a2 = abinop cop op (constOf a1) (constOf a2)

buildBinOp :: FFIBinOp -> FFI.ValueRef -> FFI.ValueRef -> CodeGenFunction (Value a)
buildBinOp op a1 a2 =
    liftM Value $
    withCurrentBuilder $ \ bld ->
      U.withEmptyCString $ op bld a1 a2

type FFIUnOp = FFI.BuilderRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef

buildUnOp :: FFIUnOp -> FFI.ValueRef -> CodeGenFunction (Value a)
buildUnOp op a =
    liftM Value $
    withCurrentBuilder $ \ bld ->
      U.withEmptyCString $ op bld a

neg :: forall a. (IsArithmetic a) => Value a -> CodeGenFunction (Value a)
neg =
    withArithmeticType $ \typ -> case typ of
      IntegerType  -> \(Value x) -> buildUnOp FFI.buildNeg x
      FloatingType -> abinop FFI.constFSub FFI.buildFSub (value zero :: Value a)

ineg :: (IsInteger a) => Value a -> CodeGenFunction (Value a)
ineg (Value x) = buildUnOp FFI.buildNeg x

fneg :: forall a. (IsFloating a) => Value a -> CodeGenFunction (Value a)
fneg = fsub (value zero :: Value a)
{-
fneg (Value x) = buildUnOp FFI.buildFNeg x
-}

inv :: (IsInteger a) => Value a -> CodeGenFunction (Value a)
inv (Value x) = buildUnOp FFI.buildNot x

--------------------------------------

-- | Get a value from a vector.
extractelement :: ((1 <=? n) ~ 'True)
               => Value (Vector n a)               -- ^ Vector
               -> Value Word32                     -- ^ Index into the vector
               -> CodeGenFunction (Value a)
extractelement (Value vec) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildExtractElement bldPtr vec i

-- | Insert a value into a vector, nondestructive.
insertelement :: ((1 <=? n) ~ 'True)
              => Value (Vector n a)                -- ^ Vector
              -> Value a                           -- ^ Value to insert
              -> Value Word32                      -- ^ Index into the vector
              -> CodeGenFunction (Value (Vector n a))
insertelement (Value vec) (Value e) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildInsertElement bldPtr vec e i

-- | Permute vector.
shufflevector :: ((1 <=? n) ~ 'True, (1 <=? m) ~ 'True)
              => Value (Vector n a)
              -> Value (Vector n a)
              -> ConstValue (Vector m Word32)
              -> CodeGenFunction (Value (Vector m a))
shufflevector (Value a) (Value b) (Value mask) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildShuffleVector bldPtr a b mask


-- |Acceptable arguments to 'extractvalue' and 'insertvalue'.
class GetValue agg ix where
    type GetValueType agg ix :: k
    getIx :: agg -> ix -> CUInt

instance (Demote i ~ Integer, IsFirstClass (GetValueType (Struct as) (Sing i))) => GetValue (Struct as) (Sing (i :: Nat)) where
    type GetValueType (Struct as) (Sing i) = FieldType as i
    getIx _ = fromIntegral . fromSing

instance (IsFirstClass a) => GetValue (Array n a) Word32 where
    type GetValueType (Array n a) Word32 = a
    getIx _ = fromIntegral

instance (IsFirstClass a) => GetValue (Array n a) Word64 where
    type GetValueType (Array n a) Word64 = a
    getIx _ = fromIntegral

instance (Demote i ~ Integer, IsFirstClass a, (i <=? n) ~ 'True) => GetValue (Array n a) (Sing i) where
    type GetValueType (Array n a) (Sing i) = a
    getIx _ = fromIntegral . fromSing

-- | Get a value from an aggregate.
extractvalue :: forall agg i.
                GetValue agg i
             => Value agg                   -- ^ Aggregate
             -> i                           -- ^ Index into the aggregate
             -> CodeGenFunction (Value (GetValueType agg i))
extractvalue (Value agg) i =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildExtractValue bldPtr agg (getIx (undefined::agg) i)

-- | Insert a value into an aggregate, nondestructive.
insertvalue :: forall agg i.
               GetValue agg i
            => Value agg                   -- ^ Aggregate
            -> Value (GetValueType agg i)  -- ^ Value to insert
            -> i                           -- ^ Index into the aggregate
            -> CodeGenFunction (Value agg)
insertvalue (Value agg) (Value e) i =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildInsertValue bldPtr agg e (getIx (undefined::agg) i)


--------------------------------------

-- XXX should allows constants

-- | Truncate a value to a shorter bit width.
trunc :: (IsInteger a, IsInteger b, IsPrimitive a, IsPrimitive b, (SizeOf b <=? (SizeOf a + 1)) ~ 'True)
      => Value a -> CodeGenFunction (Value b)
trunc = convert FFI.buildTrunc

-- | Zero extend a value to a wider width.
zext :: (IsInteger a, IsInteger b, IsPrimitive a, IsPrimitive b, (SizeOf a <=? (SizeOf b + 1)) ~ 'True)
     => Value a -> CodeGenFunction (Value b)
zext = convert FFI.buildZExt

-- | Sign extend a value to wider width.
sext :: (IsInteger a, IsInteger b, IsPrimitive a, IsPrimitive b, (SizeOf a <=? (SizeOf b + 1)) ~ 'True)
     => Value a -> CodeGenFunction (Value b)
sext = convert FFI.buildSExt

-- | Truncate a floating point value.
fptrunc :: (IsFloating a, IsFloating b, IsPrimitive a, IsPrimitive b, (SizeOf b <=? (SizeOf b + 1)) ~ 'True)
        => Value a -> CodeGenFunction (Value b)
fptrunc = convert FFI.buildFPTrunc

-- | Extend a floating point value.
fpext :: (IsFloating a, IsFloating b, IsPrimitive a, IsPrimitive b, (SizeOf a <=? (SizeOf b + 1)) ~ 'True)
      => Value a -> CodeGenFunction (Value b)
fpext = convert FFI.buildFPExt

{-# DEPRECATED fptoui "use fptoint since it is type-safe with respect to signs" #-}
-- | Convert a floating point value to an unsigned integer.
fptoui :: (IsFloating a, IsInteger b, NumberOfElements a ~ NumberOfElements b) => Value a -> CodeGenFunction (Value b)
fptoui = convert FFI.buildFPToUI

{-# DEPRECATED fptosi "use fptoint since it is type-safe with respect to signs" #-}
-- | Convert a floating point value to a signed integer.
fptosi :: (IsFloating a, IsInteger b, NumberOfElements a ~ NumberOfElements b) => Value a -> CodeGenFunction (Value b)
fptosi = convert FFI.buildFPToSI

-- | Convert a floating point value to an integer.
-- It is mapped to @fptosi@ or @fptoui@ depending on the type @a@.
fptoint :: forall a b. (IsFloating a, IsInteger b, NumberOfElements a ~ NumberOfElements b) => Value a -> CodeGenFunction (Value b)
fptoint =
   if isSigned (Proxy :: Proxy b)
     then convert FFI.buildFPToSI
     else convert FFI.buildFPToUI


{-# DEPRECATED uitofp "use inttofp since it is type-safe with respect to signs" #-}
-- | Convert an unsigned integer to a floating point value.
uitofp :: (IsInteger a, IsFloating b, NumberOfElements a ~ NumberOfElements b) => Value a -> CodeGenFunction (Value b)
uitofp = convert FFI.buildUIToFP

{-# DEPRECATED sitofp "use inttofp since it is type-safe with respect to signs" #-}
-- | Convert a signed integer to a floating point value.
sitofp :: (IsInteger a, IsFloating b, NumberOfElements a ~ NumberOfElements b) => Value a -> CodeGenFunction (Value b)
sitofp = convert FFI.buildSIToFP

-- | Convert an integer to a floating point value.
-- It is mapped to @sitofp@ or @uitofp@ depending on the type @a@.
inttofp :: forall a b. (IsInteger a, IsFloating b, NumberOfElements a ~ NumberOfElements b) => Value a -> CodeGenFunction (Value b)
inttofp =
   if isSigned (Proxy :: Proxy a)
     then convert FFI.buildSIToFP
     else convert FFI.buildUIToFP


-- | Convert a pointer to an integer.
ptrtoint :: (IsInteger b, IsPrimitive b) => Value (Ptr a) -> CodeGenFunction (Value b)
ptrtoint = convert FFI.buildPtrToInt

-- | Convert an integer to a pointer.
inttoptr :: (IsInteger a, IsType b) => Value a -> CodeGenFunction (Value (Ptr b))
inttoptr = convert FFI.buildIntToPtr

-- | Convert between to values of the same size by just copying the bit pattern.
bitcast :: (IsFirstClass a, IsFirstClass b, SizeOf a ~ SizeOf b)
        => Value a -> CodeGenFunction (Value b)
bitcast = convert FFI.buildBitCast

-- | Same as bitcast but instead of the '(:==:)' type class it uses type unification.
-- This way, properties like reflexivity, symmetry and transitivity
-- are obvious to the Haskell compiler.
{-# DEPRECATED bitcastUnify "Use bitcast instead" #-}
bitcastUnify :: (IsFirstClass a, IsFirstClass b, SizeOf a ~ SizeOf b)
        => Value a -> CodeGenFunction (Value b)
bitcastUnify = bitcast

type FFIConvert = FFI.BuilderRef -> FFI.ValueRef -> FFI.TypeRef -> U.CString -> IO FFI.ValueRef

convert :: forall a b . (IsType b) => FFIConvert -> Value a -> CodeGenFunction (Value b)
convert conv (Value a) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ conv bldPtr a (typeRef (Proxy :: Proxy b))

--------------------------------------

data CmpPredicate =
    CmpEQ                       -- ^ equal
  | CmpNE                       -- ^ not equal
  | CmpGT                       -- ^ greater than
  | CmpGE                       -- ^ greater or equal
  | CmpLT                       -- ^ less than
  | CmpLE                       -- ^ less or equal
    deriving (Eq, Ord, Enum, Show, Typeable)

uintFromCmpPredicate :: CmpPredicate -> IntPredicate
uintFromCmpPredicate p =
   case p of
      CmpEQ -> IntEQ
      CmpNE -> IntNE
      CmpGT -> IntUGT
      CmpGE -> IntUGE
      CmpLT -> IntULT
      CmpLE -> IntULE

sintFromCmpPredicate :: CmpPredicate -> IntPredicate
sintFromCmpPredicate p =
   case p of
      CmpEQ -> IntEQ
      CmpNE -> IntNE
      CmpGT -> IntSGT
      CmpGE -> IntSGE
      CmpLT -> IntSLT
      CmpLE -> IntSLE

fpFromCmpPredicate :: CmpPredicate -> FPPredicate
fpFromCmpPredicate p =
   case p of
      CmpEQ -> FPOEQ
      CmpNE -> FPONE
      CmpGT -> FPOGT
      CmpGE -> FPOGE
      CmpLT -> FPOLT
      CmpLE -> FPOLE


data IntPredicate =
    IntEQ                       -- ^ equal
  | IntNE                       -- ^ not equal
  | IntUGT                      -- ^ unsigned greater than
  | IntUGE                      -- ^ unsigned greater or equal
  | IntULT                      -- ^ unsigned less than
  | IntULE                      -- ^ unsigned less or equal
  | IntSGT                      -- ^ signed greater than
  | IntSGE                      -- ^ signed greater or equal
  | IntSLT                      -- ^ signed less than
  | IntSLE                      -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show, Typeable)

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)

toIntPredicate :: Int -> IntPredicate
toIntPredicate p = toEnum $ fromIntegral p - 32

data FPPredicate =
    FPFalse           -- ^ Always false (always folded)
  | FPOEQ             -- ^ True if ordered and equal
  | FPOGT             -- ^ True if ordered and greater than
  | FPOGE             -- ^ True if ordered and greater than or equal
  | FPOLT             -- ^ True if ordered and less than
  | FPOLE             -- ^ True if ordered and less than or equal
  | FPONE             -- ^ True if ordered and operands are unequal
  | FPORD             -- ^ True if ordered (no nans)
  | FPUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | FPUEQ             -- ^ True if unordered or equal
  | FPUGT             -- ^ True if unordered or greater than
  | FPUGE             -- ^ True if unordered, greater than, or equal
  | FPULT             -- ^ True if unordered or less than
  | FPULE             -- ^ True if unordered, less than, or equal
  | FPUNE             -- ^ True if unordered or not equal
  | FPT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show, Typeable)

fromFPPredicate :: FPPredicate -> CInt
fromFPPredicate p = fromIntegral (fromEnum p)

toFPPredicate :: Int -> FPPredicate
toFPPredicate p = toEnum $ fromIntegral p

-- |Acceptable operands to comparison instructions.
class CmpRet (CmpOpType a b) => CmpOp a b where
    type CmpOpType a b :: *
    cmpop :: FFIBinOp -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b)) -- CmpRetType (CmpOpType a b)))

instance CmpRet a => CmpOp (Value a) (Value a) where
    type CmpOpType (Value a) (Value a) = a
    cmpop op (Value a1) (Value a2) = buildBinOp op a1 a2

instance (CmpRet a, IsConst a) => CmpOp a (Value a) where
    type CmpOpType a (Value a) = a
    cmpop op a1 a2 = cmpop op (valueOf a1) a2

instance (CmpRet a, IsConst a) => CmpOp (Value a) a where
    type CmpOpType (Value a) a = a
    cmpop op a1 a2 = cmpop op a1 (valueOf a2)

type CmpOpRetType a b = CmpRetType (CmpOpType a b)

class CmpRet c where
    type CmpRetType c :: *
    cmpBld :: Proxy c -> CmpPredicate -> FFIBinOp

instance CmpRet Float where
    type CmpRetType Float = Bool
    cmpBld _ = fcmpBld

instance CmpRet Double where
    type CmpRetType Double = Bool
    cmpBld _ = fcmpBld

instance CmpRet FP128 where
    type CmpRetType FP128 = Bool
    cmpBld _ = fcmpBld

instance CmpRet Bool where
    type CmpRetType Bool = Bool
    cmpBld _ = ucmpBld

instance CmpRet Word8 where
    type CmpRetType Word8 = Bool
    cmpBld _ = ucmpBld

instance CmpRet Word16 where
    type CmpRetType Word16 = Bool
    cmpBld _ = ucmpBld

instance CmpRet Word32 where
    type CmpRetType Word32 = Bool
    cmpBld _ = ucmpBld

instance CmpRet Word64 where
    type CmpRetType Word64 = Bool
    cmpBld _ = ucmpBld

instance CmpRet Int8 where
    type CmpRetType Int8 = Bool
    cmpBld _ = scmpBld

instance CmpRet Int16 where
    type CmpRetType Int16 = Bool
    cmpBld _ = scmpBld

instance CmpRet Int32 where
    type CmpRetType Int32 = Bool
    cmpBld _ = scmpBld

instance CmpRet Int64 where
    type CmpRetType Int64 = Bool
    cmpBld _ = scmpBld

instance CmpRet (Ptr a) where
    type CmpRetType (Ptr a) = Bool
    cmpBld _ = ucmpBld

instance (CmpRet a, IsPrimitive a, (1 <=? n) ~ 'True) => CmpRet (Vector n a) where
    type CmpRetType (Vector n a) = Vector n (CmpRetType a)
    cmpBld _ = cmpBld (Proxy :: Proxy a)

{- |
Compare values of ordered types
and choose predicates according to the compared types.
Floating point numbers are compared in \"ordered\" mode,
that is @NaN@ operands yields 'False' as result.
Pointers are compared unsigned.
These choices are consistent with comparison in plain Haskell.
-}
cmp :: forall a b.
   (CmpOp a b) =>
   CmpPredicate -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b))
cmp p = cmpop (cmpBld (Proxy :: Proxy (CmpOpType a b)) p)

ucmpBld :: CmpPredicate -> FFIBinOp
ucmpBld p = flip FFI.buildICmp (fromIntPredicate (uintFromCmpPredicate p))

scmpBld :: CmpPredicate -> FFIBinOp
scmpBld p = flip FFI.buildICmp (fromIntPredicate (sintFromCmpPredicate p))

fcmpBld :: CmpPredicate -> FFIBinOp
fcmpBld p = flip FFI.buildFCmp (fromFPPredicate (fpFromCmpPredicate p))


_ucmp :: (CmpOp a b) =>
        CmpPredicate -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b))
_ucmp p = cmpop (flip FFI.buildICmp (fromIntPredicate (uintFromCmpPredicate p)))

_scmp :: (CmpOp a b) =>
        CmpPredicate -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b))
_scmp p = cmpop (flip FFI.buildICmp (fromIntPredicate (sintFromCmpPredicate p)))

pcmp :: (CmpOp a b, CmpOpType a b ~ Ptr c) =>
        IntPredicate -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b))
pcmp p = cmpop (flip FFI.buildICmp (fromIntPredicate p))


{-# DEPRECATED icmp "use cmp or pcmp instead" #-}
-- | Compare integers.
icmp :: (CmpOp a b, IsIntegerOrPointer (CmpOpType a b)) =>
        IntPredicate -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b))
icmp p = cmpop (flip FFI.buildICmp (fromIntPredicate p))

-- | Compare floating point values.
fcmp :: (CmpOp a b, IsFloating (CmpOpType a b)) =>
        FPPredicate -> a -> b -> CodeGenFunction (Value (CmpOpRetType a b))
fcmp p = cmpop (flip FFI.buildFCmp (fromFPPredicate p))

--------------------------------------

-- XXX could do const song and dance
-- | Select between two values depending on a boolean.
select :: (IsFirstClass a, CmpRet a) => Value (CmpRetType a) -> Value a -> Value a -> CodeGenFunction (Value a)
select (Value cnd) (Value thn) (Value els) =
    liftM Value $
      withCurrentBuilder $ \ bldPtr ->
        U.withEmptyCString $
          FFI.buildSelect bldPtr cnd thn els

--------------------------------------

type Caller = FFI.BuilderRef -> [FFI.ValueRef] -> IO FFI.ValueRef

-- |Acceptable arguments to 'call'.
class CallArgs (f :: *) (g :: *) | g -> f, f -> g where
    doCall :: Caller -> [FFI.ValueRef] -> f -> g

instance (CallArgs b b') => CallArgs (a -> b) (Value a -> b') where
    doCall mkCall args f (Value arg) = doCall mkCall (arg : args) (f (undefined :: a))

--instance (CallArgs b b') => CallArgs (a -> b) (ConstValue a -> b') where
--    doCall mkCall args f (ConstValue arg) = doCall mkCall (arg : args) (f (undefined :: a))

instance CallArgs (IO a) (CodeGenFunction (Value a)) where
    doCall = doCallDef

doCallDef :: Caller -> [FFI.ValueRef] -> b -> CodeGenFunction (Value a)
doCallDef mkCall args _ =
    withCurrentBuilder $ \ bld ->
      liftM Value $ mkCall bld (reverse args)

-- | Call a function with the given arguments.  The 'call' instruction is variadic, i.e., the number of arguments
-- it takes depends on the type of /f/.
-- This also sets the calling convention of the call to the function.
-- As LLVM itself defines, if the calling conventions of the calling
-- /instruction/ and the function being /called/ are different, undefined
-- behavior results.
call :: forall cconv f g . (CallArgs f g, ReifyCallingConvention cconv) => Function cconv f -> g
call (Function (Value f)) =
    let cc = reifyCallingConvention (Proxy :: Proxy cconv) in
    doCall (U.makeCallWithCc cc f) [] (undefined :: f)

-- | Call a function with exception handling.
-- This also sets the calling convention of the call to the function.
-- As LLVM itself defines, if the calling conventions of the calling
-- /instruction/ and the function being /called/ are different, undefined
-- behavior results.
invoke :: forall cconv f g . (CallArgs f g, ReifyCallingConvention cconv)
       => BasicBlock         -- ^Normal return point.
       -> BasicBlock         -- ^Exception return point.
       -> Function cconv f   -- ^Function to call.
       -> g
invoke (BasicBlock norm) (BasicBlock expt) (Function (Value f)) =
    let cc = reifyCallingConvention (Proxy :: Proxy cconv) in
    doCall (U.makeInvokeWithCc cc norm expt f) [] (undefined :: f)

--------------------------------------

-- XXX could do const song and dance
-- |Join several variables (virtual registers) from different basic blocks into one.
-- All of the variables in the list are joined.  See also 'addPhiInputs'.
phi :: forall a . (IsFirstClass a) => [(Value a, BasicBlock)] -> CodeGenFunction (Value a)
phi incoming =
    liftM Value $
      withCurrentBuilder $ \ bldPtr -> do
        inst <- U.buildEmptyPhi bldPtr (typeRef (Proxy :: Proxy a))
        U.addPhiIns inst [ (v, b) | (Value v, BasicBlock b) <- incoming ]
        return inst

-- |Add additional inputs to an existing phi node.
-- The reason for this instruction is that sometimes the structure of the code
-- makes it impossible to have all variables in scope at the point where you need the phi node.
addPhiInputs :: (IsFirstClass a)
             => Value a                      -- ^Must be a variable from a call to 'phi'.
             -> [(Value a, BasicBlock)]      -- ^Variables to add.
             -> CodeGenFunction ()
addPhiInputs (Value inst) incoming =
    liftIO $ U.addPhiIns inst [ (v, b) | (Value v, BasicBlock b) <- incoming ]


--------------------------------------

-- | Acceptable argument to array memory allocation.
class AllocArg a where
    getAllocArg :: a -> Value Word32
instance AllocArg (Value Word32) where
    getAllocArg = id
instance AllocArg (ConstValue Word32) where
    getAllocArg = value
instance AllocArg Word32 where
    getAllocArg = valueOf

-- could be moved to Util.Memory
-- FFI.buildMalloc deprecated since LLVM-2.7
-- XXX What's the type returned by malloc
-- | Allocate heap memory.
malloc :: (IsType a, SizeOf a ~ s) => CodeGenFunction (Value (Ptr a))
malloc = arrayMalloc (1::Word32)

{-
I use a pointer type as size parameter of 'malloc'.
This way I hope that the parameter has always the correct size (32 or 64 bit).
A side effect is that we can convert the result of 'getelementptr' using 'bitcast',
that does not suffer from the slow assembly problem. (bug #8281)
-}
foreign import ccall "&aligned_malloc_sizeptr"
   alignedMalloc :: FunPtr (Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8))

foreign import ccall "&aligned_free"
   alignedFree :: FunPtr (Ptr Word8 -> IO ())


{-
There is a bug in LLVM-2.7 and LLVM-2.8
(http://llvm.org/bugs/show_bug.cgi?id=8281)
that causes huge assembly times for expressions like
ptrtoint(getelementptr(zero,..)).
If you break those expressions into two statements
at separate lines, everything is fine.
But the C interface is too clever,
and rewrites two separate statements into a functional expression on a single line.
Such code is generated whenever you call
buildMalloc, buildArrayMalloc, sizeOf (called by buildMalloc), or alignOf.
One possible way is to write a getelementptr expression
containing a nullptr in a way
that hides the constant nature of nullptr.

    ptr <- alloca
    store (value zero) ptr
    z <- load ptr
    size <- bitcastUnify =<<
       getElementPtr (z :: Value (Ptr a)) (getAllocArg s, ())

However, I found that bitcast on pointers causes no problems.
Thus I switched to using pointers for size quantities.
This still allows for optimizations involving pointers.
-}

-- XXX What's the type returned by arrayMalloc?
-- | Allocate heap (array) memory.
arrayMalloc :: forall a s . (IsType a, AllocArg s) =>
               s -> CodeGenFunction (Value (Ptr a)) -- XXX
arrayMalloc s = do
    func <- staticFunction alignedMalloc
--    func <- externFunction "malloc"

    size <- sizeOfArray (Proxy :: Proxy a) (getAllocArg s)
    alignment <- alignOf (Proxy :: Proxy a)
    bitcast =<<
       call
          (func :: Function 'FFI.C (Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)))
          size
          alignment

-- XXX What's the type returned by malloc
-- | Allocate stack memory.
alloca :: forall a . (IsType a) => CodeGenFunction (Value (Ptr a))
alloca =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildAlloca bldPtr (typeRef (Proxy :: Proxy a))

-- XXX What's the type returned by arrayAlloca?
-- | Allocate stack (array) memory.
arrayAlloca :: forall a s . (IsType a, AllocArg s) =>
               s -> CodeGenFunction (Value (Ptr a))
arrayAlloca s =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildArrayAlloca bldPtr (typeRef (Proxy :: Proxy a)) (case getAllocArg s of Value v -> v)

-- FFI.buildFree deprecated since LLVM-2.7
-- XXX What's the type of free?
-- | Free heap memory.
free :: (IsType a) => Value (Ptr a) -> CodeGenFunction ()
free ptr = do
    func <- staticFunction alignedFree
--    func <- externFunction "free"
    _ <- call (func :: Function 'FFI.C (Ptr Word8 -> IO ())) =<< bitcast ptr
    return ()


-- | If we want to export that, then we should have a Size type
-- This is the official implementation,
-- but it suffers from the ptrtoint(gep) bug.
sizeOf :: forall a . (IsType a) => a -> CodeGenFunction (Value Word64)
sizeOf _ =
    liftIO . liftM Value $
    FFI.sizeOf (typeRef (Proxy :: Proxy a))

alignOf :: forall a . IsType a => Proxy a -> CodeGenFunction (Value (Ptr Word8))
alignOf _ = do
    x <- liftIO . liftM Value $ FFI.alignOf (typeRef (Proxy :: Proxy a))
    inttoptr (x :: Value Word64)

-- Here are reimplementation from Constants.cpp that avoid the ptrtoint(gep) bug #8281.
-- see ConstantExpr::getSizeOf
sizeOfArray :: forall (a :: *) . (IsType a) => Proxy a -> Value Word32 -> CodeGenFunction (Value (Ptr Word8))
sizeOfArray _ len =
    bitcast =<<
       getElementPtr (value zero :: Value (Ptr a)) (len, ())

-- see ConstantExpr::getAlignOf
-- alignOf :: forall a s . (IsType a) => a -> CodeGenFunction (Value (Ptr Word8))

{-
alignOf :: forall a . (IsType a, (GetElementPtr (Struct [Bool, a]) (Sing 1, ())))
        => a -> CodeGenFunction (Value (Ptr Word8))
alignOf _ = undefined
    bitcast =<<
       getElementPtr0 (value zero :: Value (Ptr (Struct [Bool, a]))) (sing :: Sing 1, ())
-}


-- | Load a value from memory.
load :: Value (Ptr a)                   -- ^ Address to load from.
     -> CodeGenFunction (Value a)
load (Value p) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildLoad bldPtr p

-- | Store a value in memory
store :: Value a                        -- ^ Value to store.
      -> Value (Ptr a)                  -- ^ Address to store to.
      -> CodeGenFunction ()
store (Value v) (Value p) = do
    withCurrentBuilder_ $ \ bldPtr ->
      FFI.buildStore bldPtr v p
    return ()

{-
-- XXX type is wrong
-- | Address arithmetic.  See LLVM description.
-- (The type isn't as accurate as it should be.)
getElementPtr :: (IsInteger i) =>
                 Value (Ptr a) -> [Value i] -> CodeGenFunction (Value (Ptr b))
getElementPtr (Value ptr) ixs =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen [ v | Value v <- ixs ] $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)
-}

-- |Acceptable single index to 'getElementPointer'.
class IsIndexArg a where
    getArg :: a -> FFI.ValueRef

instance IsIndexArg (Value Word32) where
    getArg (Value v) = v

instance IsIndexArg (Value Word64) where
    getArg (Value v) = v

instance IsIndexArg (Value Int32) where
    getArg (Value v) = v

instance IsIndexArg (Value Int64) where
    getArg (Value v) = v

instance IsIndexArg (ConstValue Word32) where
    getArg = unConst

instance IsIndexArg (ConstValue Word64) where
    getArg = unConst

instance IsIndexArg (ConstValue Int32) where
    getArg = unConst

instance IsIndexArg (ConstValue Int64) where
    getArg = unConst

instance IsIndexArg Word32 where
    getArg = unConst . constOf

instance IsIndexArg Word64 where
    getArg = unConst . constOf

instance IsIndexArg Int32 where
    getArg = unConst . constOf

instance IsIndexArg Int64 where
    getArg = unConst . constOf

unConst :: ConstValue a -> FFI.ValueRef
unConst = unValue

-- End of indexing

-- |Acceptable arguments to 'getElementPointer'.
class GetElementPtr optr ixs where
    type GetElementPtrType optr ixs :: *
    getIxList :: Proxy optr -> ixs -> [FFI.ValueRef]

instance GetElementPtr (a :: *) () where
    type GetElementPtrType a () = a
    getIxList _ () = []

-- Index in Array
instance (GetElementPtr o i, IsIndexArg a) => GetElementPtr (Array k o) (a, i) where
    type GetElementPtrType (Array k o) (a, i) = GetElementPtrType o i
    getIxList _ (v, i) = getArg v : getIxList (Proxy :: Proxy o) i

-- Index in Vector
instance (GetElementPtr o i, IsIndexArg a, (1 <=? k) ~ 'True) => GetElementPtr (Vector k o) (a, i) where
    type GetElementPtrType (Vector k o) (a, i) = GetElementPtrType o i
    getIxList _ (v, i) = getArg v : getIxList (Proxy :: Proxy o) i

-- Index in Struct and PackedStruct.
-- The index has to be a type level integer to statically determine the record field type
instance (GetElementPtr (FieldType fs a) i, Demote a ~ Integer) => GetElementPtr (Struct fs) (Sing (a :: Nat), i) where
    type GetElementPtrType (Struct fs) (Sing a, i) = GetElementPtrType (FieldType fs a) i
    getIxList _ (v, i) = x:xs where
      x  = unConst (constOf (fromIntegral (fromSing v) :: Word32))
      xs = getIxList (Proxy :: Proxy (FieldType fs a)) i

instance (GetElementPtr (FieldType fs a) i, Demote a ~ Integer) => GetElementPtr (PackedStruct fs) (Sing (a :: Nat), i) where
    type GetElementPtrType (PackedStruct fs) (Sing a, i) = GetElementPtrType (FieldType fs a) i
    getIxList _ (v, i) = x:xs where
      x  = unConst (constOf (fromIntegral (fromSing v) :: Word32))
      xs = getIxList (Proxy :: Proxy (FieldType fs a)) i

type family FieldType (as :: [*]) (i :: Nat) :: *
type instance FieldType xs i = FieldRecursion xs (i <=? 0) i

type family FieldRecursion (as :: [*]) (f :: Bool) (i :: Nat) :: *
type instance FieldRecursion (x ': xs) 'True  i = x
type instance FieldRecursion (x ': xs) 'False i = FieldType xs (i - 1)

-- | Address arithmetic.  See LLVM description.
-- The index is a nested tuple of the form @(i1,(i2,( ... ())))@.
-- (This is without a doubt the most confusing LLVM instruction, but the types help.)
getElementPtr :: forall a o i . (GetElementPtr o i, IsIndexArg a) =>
                 Value (Ptr o) -> (a, i) -> CodeGenFunction (Value (Ptr (GetElementPtrType o i)))
getElementPtr (Value ptr) (a, ixs) =
    let ixl = getArg a : getIxList (Proxy :: Proxy o) ixs in
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen ixl $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)

-- | Like getElementPtr, but with an initial index that is 0.
-- This is useful since any pointer first need to be indexed off the pointer, and then into
-- its actual value.  This first indexing is often with 0.
getElementPtr0 :: (GetElementPtr o i) =>
                  Value (Ptr o) -> i -> CodeGenFunction (Value (Ptr (GetElementPtrType o i)))
getElementPtr0 p i = getElementPtr p (0::Word32, i)

--------------------------------------
{-
instance (IsConst a) => Show (ConstValue a) -- XXX
instance (IsConst a) => Eq (ConstValue a)

{-
instance (IsConst a) => Eq (ConstValue a) where
    ConstValue x == ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOEQ) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntEQ) x y)
    ConstValue x /= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPONE) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntNE) x y)

instance (IsConst a) => Ord (ConstValue a) where
    ConstValue x <  ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOLT) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntLT) x y)
    ConstValue x <= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOLE) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntLE) x y)
    ConstValue x >  ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOGT) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntGT) x y)
    ConstValue x >= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOGE) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntGE) x y)
-}

instance (Num a, IsConst a) => Num (ConstValue a) where
    ConstValue x + ConstValue y  =  ConstValue (FFI.constAdd x y)
    ConstValue x - ConstValue y  =  ConstValue (FFI.constSub x y)
    ConstValue x * ConstValue y  =  ConstValue (FFI.constMul x y)
    negate (ConstValue x)        =  ConstValue (FFI.constNeg x)
    fromInteger x                =  constOf (fromInteger x :: a)
-}
