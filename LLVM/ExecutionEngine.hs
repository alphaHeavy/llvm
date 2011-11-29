{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
 -- |An 'ExecutionEngine' is JIT compiler that is used to generate code for an LLVM module.
module LLVM.ExecutionEngine(
    -- * Execution engine
    ExecutionEngine,
    createExecutionEngine,
    destroyExecutionEngine,
    touchExecutionEngine,
    addModule,
    removeModule,
    runStaticConstructors,
    runStaticDestructors,
    getPointerToFunction,
    addFunctionValue,
    addGlobalMappings,
    getFreePointers, FreePointers,
    c_freeFunctionObject,
    -- * Translation
    Translatable, Generic,
    -- * Target information
    module LLVM.ExecutionEngine.Target
    ) where

import LLVM.ExecutionEngine.Engine
import LLVM.FFI.Core (ValueRef)
import LLVM.ExecutionEngine.Target

-- |Class of LLVM function types that can be translated to the corresponding
-- Haskell type.
class Translatable f where
  translate :: (ValueRef -> [GenericValue] -> IO GenericValue) -> [GenericValue] -> ValueRef -> f

instance (Generic a, Translatable b) => Translatable (a -> b) where
  translate run args f = \ arg -> translate run (toGeneric arg : args) f

instance (Generic a) => Translatable (IO a) where
  translate run args f = fmap fromGeneric $ run f $ reverse args

