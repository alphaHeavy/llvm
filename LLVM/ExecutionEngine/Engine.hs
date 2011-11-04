{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, UndecidableInstances, OverlappingInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.ExecutionEngine.Engine(
       EngineAccess,
       runEngineAccess,
{-
       ExecutionEngine,
-}
       createExecutionEngine, addModule, removeModule,
       {- runStaticConstructors, runStaticDestructors, -}
       getExecutionEngineTargetData,
       getPointerToFunction,
       addFunctionValue, addGlobalMappings,
       getFreePointers, FreePointers,
       c_freeFunctionObject,
       runFunction, getRunFunction,
       GenericValue, Generic(..)
       ) where
import Control.Monad.State
import Control.Applicative (Applicative, )
import Control.Concurrent.MVar
import Data.Typeable
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (fromBool)
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr)
import LLVM.Core.CodeGen(Value(..), Function)
import LLVM.Core.CodeGenMonad(GlobalMappings(..))
import Foreign.Storable (peek)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, castPtrToStablePtr, )
import System.IO.Unsafe (unsafePerformIO)

import LLVM.Core.Util(Module, createModule)
import qualified LLVM.FFI.ExecutionEngine as FFI
import qualified LLVM.FFI.Target as FFI
import qualified LLVM.FFI.Core as FFI(ValueRef)
import qualified LLVM.Core.Util as U
import LLVM.Core.Type(IsFirstClass, typeRef)

{-
-- |The type of the JITer.
newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr FFI.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr FFI.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine = withForeignPtr . fromExecutionEngine

-- |Create an execution engine for a module provider.
-- Warning, do not call this function more than once.
createExecutionEngine :: ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov =
    withModuleProvider prov $ \provPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- FFI.createExecutionEngine eePtr provPtr errPtr
          if ret == 1
            then do err <- peek errPtr
                    errStr <- peekCString err
                    free err
                    ioError . userError $ errStr
            else do ptr <- peek eePtr
                    liftM ExecutionEngine $ newForeignPtr FFI.ptrDisposeExecutionEngine ptr

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee FFI.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee FFI.runStaticDestructors

getExecutionEngineTargetData :: ExecutionEngine -> IO FFI.TargetDataRef
getExecutionEngineTargetData ee = withExecutionEngine ee FFI.getExecutionEngineTargetData

getPointerToFunction :: ExecutionEngine -> Function f -> IO (FunPtr f)
getPointerToFunction ee (Value f) =
    withExecutionEngine ee $ \ eePtr ->
      FFI.getPointerToGlobal eePtr f
-}

-- This global variable holds the one and only execution engine.
-- It may be missing, but it never dies.
-- XXX We could provide a destructor, what about functions obtained by runFunction?
{-# NOINLINE theEngine #-}
theEngine :: MVar (Maybe (Ptr FFI.ExecutionEngine))
theEngine = unsafePerformIO $ newMVar Nothing

createExecutionEngine :: Module -> IO (Ptr FFI.ExecutionEngine)
createExecutionEngine m =
    U.withModule m $ \ mPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          failed <- FFI.createJITCompilerForModule eePtr mPtr 2 errPtr
          if failed
            then do
                err <- peek errPtr
                errStr <- peekCString err
                free err
                ioError . userError $ errStr
            else
                peek eePtr

getTheEngine :: IO (Ptr FFI.ExecutionEngine)
getTheEngine = do
    mee <- takeMVar theEngine
    case mee of
        Just ee -> do putMVar theEngine mee; return ee
        Nothing -> do
            m <- createModule "__empty__"
            ee <- createExecutionEngine m
            putMVar theEngine (Just ee)
            return ee

data EAState = EAState {
    ea_engine :: Ptr FFI.ExecutionEngine,
    ea_modules :: [Module]
    }
    deriving (Show, Typeable)

newtype EngineAccess a = EA (StateT EAState IO a)
    deriving (Functor, Applicative, Monad, MonadState EAState, MonadIO)

-- |The LLVM execution engine is encapsulated so it cannot be accessed directly.
-- The reason is that (currently) there must only ever be one engine,
-- so access to it is wrapped in a monad.
runEngineAccess :: EngineAccess a -> IO a
runEngineAccess (EA body) = do
    eePtr <- getTheEngine
    let ea = EAState { ea_engine = eePtr, ea_modules = [] }
    (a, _ea') <- runStateT body ea
    -- XXX should remove module providers again
    return a

getExecutionEngineTargetData :: EngineAccess FFI.TargetDataRef
getExecutionEngineTargetData = do
    eePtr <- gets ea_engine
    liftIO $ FFI.getExecutionEngineTargetData eePtr

{- |
In contrast to 'generateFunction' this compiles a function once.
Thus it is faster for many calls to the same function.
See @examples\/Vector.hs@.

If the function calls back into Haskell code,
you also have to set the function addresses
using 'addFunctionValue' or 'addGlobalMappings'.
-}
getPointerToFunction :: Function f -> EngineAccess (FunPtr f)
getPointerToFunction (Value f) = do
    eePtr <- gets ea_engine
    liftIO $ FFI.getPointerToGlobal eePtr f

{- |
Tell LLVM the address of an external function
if it cannot resolve a name automatically.
Alternatively you may declare the function
with 'staticFunction' instead of 'externFunction'.
-}
addFunctionValue :: Function f -> FunPtr f -> EngineAccess ()
addFunctionValue (Value g) f =
    addFunctionValueCore g (castFunPtrToPtr f)

{- |
Pass a list of global mappings to LLVM
that can be obtained from 'LLVM.Core.getGlobalMappings'.
-}
addGlobalMappings :: GlobalMappings -> EngineAccess ()
addGlobalMappings (GlobalMappings gms) =
    mapM_ (uncurry addFunctionValueCore) gms

addFunctionValueCore :: U.Function -> Ptr () -> EngineAccess ()
addFunctionValueCore g f = do
    eePtr <- gets ea_engine
    liftIO $ FFI.addGlobalMapping eePtr g f

addModule :: Module -> EngineAccess ()
addModule m = do
    eePtr <- gets ea_engine
    liftIO $ U.withModule m $ \ mPtr -> do
        FFI.addModule eePtr mPtr

removeModule :: Module -> EngineAccess ()
removeModule m = do
    eePtr <- gets ea_engine
    liftIO $ U.withModule m $ \ mPtr ->
        alloca $ \ unused1 ->
            alloca $ \ unused2 -> do
                _ <- FFI.removeModule eePtr mPtr unused1 unused2
                return ()

-- | Get all the information needed to free a function.
-- Freeing code might have to be done from a (C) finalizer, so it has to done from C.
-- The function c_freeFunctionObject take these pointers as arguments and frees the function.
type FreePointers = (Ptr FFI.ExecutionEngine, FFI.ValueRef)
getFreePointers :: Function f -> EngineAccess FreePointers
getFreePointers (Value f) = do
    ea <- get
    return (ea_engine ea, f)

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

runFunction :: U.Function -> [GenericValue] -> EngineAccess GenericValue
runFunction func args = do
    eePtr <- gets ea_engine
    liftIO $ withAll args $ \argLen argPtr ->
                 createGenericValueWith $ FFI.runFunction eePtr func
                                              (fromIntegral argLen) argPtr
getRunFunction :: EngineAccess (U.Function -> [GenericValue] -> IO GenericValue)
getRunFunction = do
    eePtr <- gets ea_engine
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
