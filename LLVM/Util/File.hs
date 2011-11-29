module LLVM.Util.File
  ( writeCodeGenModule
  , optimizeFunction
  ) where

import Control.Monad.Trans
import System.Cmd (system)

import LLVM.Core
import LLVM.ExecutionEngine

writeCodeGenModule :: MonadIO m => FilePath -> CodeGenModule m a -> m ()
writeCodeGenModule name f = do
  m <- newModule
  _ <- defineModule m f
  liftIO $ writeBitcodeToFile name m

optimize :: FilePath -> IO ()
optimize name = do
  _rc <- system $ "opt -std-compile-opts " ++ name ++ " -f -o " ++ name
  return ()

optimizeFunction :: (Functor m, MonadIO m, IsType t, Translatable t) => CodeGenModule m (Function t) -> m (Function t)
optimizeFunction = fmap snd . optimizeFunction'

optimizeFunction' :: (MonadIO m, IsType t, Translatable t) => CodeGenModule m (Function t) -> m (Module, Function t)
optimizeFunction' mdl = do
  m <- newModule
  mf <- defineModule m mdl
  fName <- getValueName mf

  let name = "__tmp__" ++ fName ++ ".bc"
  liftIO $ writeBitcodeToFile name m

  liftIO $ optimize name

  m' <- liftIO $ readBitcodeFromFile name
  funcs <- liftIO $ getModuleValues m'

--    removeFile name

  let Just mf' = castModuleValue =<< lookup fName funcs

  return (m', mf')

