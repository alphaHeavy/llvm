{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Core.CodeGenMonad(
    -- * Module code generation
    CodeGenModule, runCodeGenModule, genMSym, getModule,
    GlobalMappings(..), addGlobalMapping, getGlobalMappings,
    -- * Function code generation
    CodeGenFunction, runCodeGenFunction, liftCodeGenModule, genFSym, getFunction, getBuilder, getFunctionModule, getExterns, putExterns,
    ) where

import Control.Monad.State (MonadState, StateT, evalStateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Applicative (Applicative)
import Data.Typeable
import Foreign.Ptr (Ptr)
import LLVM.Core.Util (Module, Builder, Function)

--------------------------------------

data CGMState = CGMState
  { cgm_module :: Module
  , cgm_externs :: [(String, Function)]
  , cgm_global_mappings :: [(Function, Ptr ())]
  , cgm_next :: !Int }
    deriving (Show, Typeable)

newtype CodeGenModule m a = CGM (StateT CGMState m a)
  deriving (Functor, Applicative, Monad, MonadState CGMState, MonadIO, MonadTrans)

genMSym :: Monad m => String -> CodeGenModule m String
genMSym prefix = do
  s <- get
  let n = cgm_next s
  put (s { cgm_next = n + 1 })
  return $ "_" ++ prefix ++ show n

getModule :: Monad m => CodeGenModule m Module
getModule = gets cgm_module

runCodeGenModule :: MonadIO m => Module -> CodeGenModule m a -> m a
runCodeGenModule m (CGM body) = do
  let cgm = CGMState { cgm_module = m, cgm_next = 1, cgm_externs = [], cgm_global_mappings = [] }
  evalStateT body cgm

--------------------------------------

data CGFState r = CGFState
  { cgf_module :: CGMState
  , cgf_builder :: Builder
  , cgf_function :: Function
  , cgf_next :: !Int
  } deriving (Show, Typeable)

newtype CodeGenFunction r m a = CGF (StateT (CGFState r) m a)
  deriving (Functor, Applicative, Monad, MonadState (CGFState r), MonadIO, MonadTrans)

genFSym :: Monad m => CodeGenFunction a m String
genFSym = do
  s <- get
  let n = cgf_next s
  put (s { cgf_next = n + 1 })
  return $ "_L" ++ show n

getFunction :: Monad m => CodeGenFunction a m Function
getFunction = gets cgf_function

getBuilder :: Monad m => CodeGenFunction a m Builder
getBuilder = gets cgf_builder

getFunctionModule :: Monad m => CodeGenFunction a m Module
getFunctionModule = gets (cgm_module . cgf_module)

getExterns :: Monad m => CodeGenFunction a m [(String, Function)]
getExterns = gets (cgm_externs . cgf_module)

putExterns :: Monad m => [(String, Function)] -> CodeGenFunction a m ()
putExterns es = do
  cgf <- get
  let cgm' = (cgf_module cgf) { cgm_externs = es }
  put (cgf { cgf_module = cgm' })

addGlobalMapping :: Monad m
                 => Function
                 -> Ptr ()
                 -> CodeGenModule m ()
addGlobalMapping value func = modify $ \cgm ->
  cgm { cgm_global_mappings = (value,func) : cgm_global_mappings cgm }

newtype GlobalMappings =
   GlobalMappings [(Function, Ptr ())]

{- |
Get a list created by calls to 'staticFunction'
that must be passed to the execution engine
via 'LLVM.ExecutionEngine.addGlobalMappings'.
-}
getGlobalMappings :: Monad m => CodeGenModule m GlobalMappings
getGlobalMappings = gets (GlobalMappings . cgm_global_mappings)

runCodeGenFunction :: MonadIO m => Builder -> Function -> CodeGenFunction r m a -> CodeGenModule m a
runCodeGenFunction bld fn (CGF body) = do
  cgm <- get
  let cgf = CGFState { cgf_module = cgm,
                       cgf_builder = bld,
                       cgf_function = fn,
                       cgf_next = 1 }
  (a, cgf') <- lift $ runStateT body cgf
  put (cgf_module cgf')
  return a

--------------------------------------

-- | Allows you to define part of a module while in the middle of defining a function.
liftCodeGenModule :: MonadIO m => CodeGenModule m a -> CodeGenFunction r m a
liftCodeGenModule (CGM act) = do
  cgf <- get
  (a, cgm') <- lift $ runStateT act (cgf_module cgf)
  put (cgf { cgf_module = cgm' })
  return a

