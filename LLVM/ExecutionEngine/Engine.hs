{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, UndecidableInstances, OverlappingInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module LLVM.ExecutionEngine.Engine(
       ExecutionEngine,
       createExecutionEngine, destroyExecutionEngine, touchExecutionEngine,
       addModule, removeModule,
       runStaticConstructors, runStaticDestructors,
       getExecutionEngineTargetData,
       getPointerToFunction,
       addFunctionValue, addGlobalMappings,
       getFreePointers, FreePointers,
       c_freeFunctionObject,
       runFunction, getRunFunction,
       GenericValue, Generic(..)
       ) where
import Control.Monad.State
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr, newForeignPtr, touchForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (fromBool)
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr)
import LLVM.Core.CodeGen(Value(..), Function)
import LLVM.Core.CodeGenMonad(GlobalMappings(..))
import Foreign.Storable (peek)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, castPtrToStablePtr, )
import System.IO.Unsafe (unsafePerformIO)

import LLVM.Core.Util (Module)
import qualified LLVM.FFI.ExecutionEngine as FFI
import qualified LLVM.FFI.Target as FFI
import qualified LLVM.FFI.Core as FFI(ValueRef)
import qualified LLVM.Core.Util as U
import LLVM.Core.Type(IsFirstClass, typeRef)

-- |The type of the JITer.
newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr FFI.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr FFI.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine = withForeignPtr . fromExecutionEngine

-- |Create an execution engine for a module provider.
-- Warning, do not call this function more than once.
createExecutionEngine :: Module -> IO ExecutionEngine
createExecutionEngine m =
    U.withModule m $ \mPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- FFI.createExecutionEngineForModule eePtr mPtr errPtr
          if ret
            then do err <- peek errPtr
                    errStr <- peekCString err
                    free err
                    ioError . userError $ errStr
            else do ptr <- peek eePtr
                    liftM ExecutionEngine $ newForeignPtr FFI.ptrDisposeExecutionEngine ptr

destroyExecutionEngine :: ExecutionEngine -> IO ()
destroyExecutionEngine = finalizeForeignPtr . fromExecutionEngine

touchExecutionEngine :: ExecutionEngine -> IO ()
touchExecutionEngine = touchForeignPtr . fromExecutionEngine

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee FFI.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee FFI.runStaticDestructors

getExecutionEngineTargetData :: ExecutionEngine -> IO FFI.TargetDataRef
getExecutionEngineTargetData ee = withExecutionEngine ee FFI.getExecutionEngineTargetData

{- |
In contrast to 'generateFunction' this compiles a function once.
Thus it is faster for many calls to the same function.
See @examples\/Vector.hs@.

If the function calls back into Haskell code,
you also have to set the function addresses
using 'addFunctionValue' or 'addGlobalMappings'.
-}
getPointerToFunction :: ExecutionEngine -> Function f -> IO (FunPtr f)
getPointerToFunction ee (Value f) =
    withExecutionEngine ee $ \ eePtr ->
      FFI.getPointerToGlobal eePtr f

{- |
Tell LLVM the address of an external function
if it cannot resolve a name automatically.
Alternatively you may declare the function
with 'staticFunction' instead of 'externFunction'.
-}
addFunctionValue :: ExecutionEngine -> Function f -> FunPtr f -> IO ()
addFunctionValue ee (Value g) f =
    addFunctionValueCore ee g (castFunPtrToPtr f)

{- |
Pass a list of global mappings to LLVM
that can be obtained from 'LLVM.Core.getGlobalMappings'.
-}
addGlobalMappings :: ExecutionEngine -> GlobalMappings -> IO ()
addGlobalMappings ee (GlobalMappings gms) =
    mapM_ (uncurry (addFunctionValueCore ee)) gms

addFunctionValueCore :: ExecutionEngine -> U.Function -> Ptr () -> IO ()
addFunctionValueCore ee g f = withExecutionEngine ee $ \eePtr ->
    FFI.addGlobalMapping eePtr g f

addModule :: ExecutionEngine -> Module -> IO ()
addModule ee m = withExecutionEngine ee $ \eePtr ->
    liftIO $ U.withModule m $ \ mPtr -> do
        FFI.addModule eePtr mPtr

removeModule :: ExecutionEngine -> Module -> IO ()
removeModule ee m = withExecutionEngine ee $ \eePtr ->
    liftIO $ U.withModule m $ \ mPtr ->
        alloca $ \ unused1 ->
            alloca $ \ unused2 -> do
                _ <- FFI.removeModule eePtr mPtr unused1 unused2
                return ()

-- | Get all the information needed to free a function.
-- Freeing code might have to be done from a (C) finalizer, so it has to done from C.
-- The function c_freeFunctionObject take these pointers as arguments and frees the function.
type FreePointers = (Ptr FFI.ExecutionEngine, FFI.ValueRef)
getFreePointers :: ExecutionEngine -> Function f -> IO FreePointers
getFreePointers ee (Value f) = withExecutionEngine ee $ \eePtr ->
    return (eePtr, f)

foreign import ccall c_freeFunctionObject :: Ptr FFI.ExecutionEngine -> FFI.ValueRef -> IO ()

--------------------------------------

newtype GenericValue = GenericValue {
      fromGenericValue :: ForeignPtr FFI.GenericValue
    }

withGenericValue :: GenericValue -> (FFI.GenericValueRef -> IO a) -> IO a
withGenericValue = withForeignPtr . fromGenericValue

createGenericValueWith :: IO FFI.GenericValueRef -> IO GenericValue
createGenericValueWith f = do
  ptr <- f
  liftM GenericValue $ newForeignPtr FFI.ptrDisposeGenericValue ptr

withAll :: [GenericValue] -> (Int -> Ptr FFI.GenericValueRef -> IO a) -> IO a
withAll ps a = go [] ps
    where go ptrs (x:xs) = withGenericValue x $ \ptr -> go (ptr:ptrs) xs
          go ptrs _ = withArrayLen (reverse ptrs) a

runFunction :: ExecutionEngine -> U.Function -> [GenericValue] -> IO GenericValue
runFunction ee func args = withExecutionEngine ee $ \eePtr ->
    withAll args $ \argLen argPtr ->
        createGenericValueWith $ FFI.runFunction eePtr func
                                              (fromIntegral argLen) argPtr
getRunFunction :: ExecutionEngine -> IO (U.Function -> [GenericValue] -> IO GenericValue)
getRunFunction ee = withExecutionEngine ee $ \eePtr -> do
    return $ \ func args ->
             withAll args $ \argLen argPtr ->
                 createGenericValueWith $ FFI.runFunction eePtr func
                                              (fromIntegral argLen) argPtr

class Generic a where
    toGeneric :: a -> GenericValue
    fromGeneric :: GenericValue -> a

instance Generic () where
    toGeneric _ = error "toGeneric ()"
    fromGeneric _ = ()

toGenericInt :: (Integral a, IsFirstClass a) => Bool -> a -> GenericValue
toGenericInt signed val = unsafePerformIO $ createGenericValueWith $
    FFI.createGenericValueOfInt (typeRef val) (fromIntegral val) (fromBool signed)

fromGenericInt :: (Integral a, IsFirstClass a) => Bool -> GenericValue -> a
fromGenericInt signed val = unsafePerformIO $
    withGenericValue val $ \ref ->
      return . fromIntegral $ FFI.genericValueToInt ref (fromBool signed)

--instance Generic Bool where
--    toGeneric = toGenericInt False . fromBool
--    fromGeneric = toBool . fromGenericInt False

instance Generic Int8 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int16 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int32 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

{-
instance Generic Int where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True
-}

instance Generic Int64 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Word8 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word16 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word32 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word64 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

toGenericReal :: (Real a, IsFirstClass a) => a -> GenericValue
toGenericReal val = unsafePerformIO $ createGenericValueWith $
    FFI.createGenericValueOfFloat (typeRef val) (realToFrac val)

fromGenericReal :: forall a . (Fractional a, IsFirstClass a) => GenericValue -> a
fromGenericReal val = unsafePerformIO $
    withGenericValue val $ \ ref ->
      return . realToFrac $ FFI.genericValueToFloat (typeRef (undefined :: a)) ref

instance Generic Float where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic Double where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic (Ptr a) where
    toGeneric = unsafePerformIO . createGenericValueWith . FFI.createGenericValueOfPointer
    fromGeneric val = unsafePerformIO . withGenericValue val $ FFI.genericValueToPointer

instance Generic (StablePtr a) where
    toGeneric = unsafePerformIO . createGenericValueWith . FFI.createGenericValueOfPointer . castStablePtrToPtr
    fromGeneric val = unsafePerformIO . fmap castPtrToStablePtr . withGenericValue val $ FFI.genericValueToPointer
