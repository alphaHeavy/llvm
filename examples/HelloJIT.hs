{-# LANGUAGE DataKinds #-}

module HelloJIT (main) where

import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

bldGreet :: CodeGenModule (Function 'C (IO ()))
bldGreet = withStringNul "Hello, JIT!" (\greetz -> do
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    func <- createFunction ExternalLinkage $ do
      tmp <- getElementPtr0 greetz (0::Word32, ())
      _ <- call puts tmp -- Throw away return value.
      ret ()
    return func)

main :: IO ()
main = do
    initializeNativeTarget
    greet <- simpleFunction bldGreet
    greet
    greet
    greet
    return ()
