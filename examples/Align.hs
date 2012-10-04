{-# LANGUAGE DataKinds #-}

module Align (main) where
import Data.Word
import Data.Proxy

import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget

    td <- getTargetData
    print (littleEndian td,
           aBIAlignmentOfType td $ typeRef (Proxy :: Proxy Word32),
           aBIAlignmentOfType td $ typeRef (Proxy :: Proxy Word64),
	   aBIAlignmentOfType td $ typeRef (Proxy :: Proxy (Vector 4 Float)),
	   aBIAlignmentOfType td $ typeRef (Proxy :: Proxy (Vector 1 Double)),
	   storeSizeOfType td $ typeRef (Proxy :: Proxy (Vector 4 Float)),
           intPtrType td
	   )
