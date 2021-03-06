{-# LANGUAGE TypeOperators, DataKinds #-}
module Vector where

import Convert
import GHC.TypeLits

import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Optimize (optimizeModule, )
import LLVM.Util.Loop (forLoop, )

import Control.Monad (liftM2, )
import Data.Word (Word32, )

-- Type of vector elements.
type T = Float

-- Number of vector elements.
type N = 16

cgvec :: CodeGenModule (Function 'C (T -> IO T))
cgvec = do
    -- A global variable that vectest messes with.
    acc <- createNamedGlobal False ExternalLinkage "acc" (constOf (0 :: T))

    -- Return the global variable.
    retAcc <- createNamedFunction ExternalLinkage "retacc" $ do
        vacc <- load acc
        ret vacc
    let _ = retAcc :: Function 'C (IO T)  -- Force the type of retAcc.

    -- A function that tests vector opreations.
    f <- createNamedFunction ExternalLinkage "vectest" $ \ x -> do

        let v = value (zero :: ConstValue (Vector 16 T))
            n = fromIntegral (fromSing (sing :: Sing 16)) :: Word32

        -- Fill the vector with x, x+1, x+2, ...
        (_, v1) <- forLoop (valueOf 0) (valueOf n) (x, v) $ \ i (x1, v1) -> do
            x1' <- add x1 (1::T)
            v1' <- insertelement v1 x1 i
            return (x1', v1')

        -- Elementwise cubing of the vector.
        vsq <- mul v1 v1
        vcb <- mul vsq v1

        -- Sum the elements of the vector.
        s <- forLoop (valueOf 0) (valueOf n) (valueOf 0) $ \ i s -> do
            y <- extractelement vcb i
            s' <- add s (y :: Value T)
            return s'

        -- Update the global variable.
        vacc <- load acc
        vacc' <- add vacc s
        store vacc' acc

        ret (s :: Value T)

--    liftIO $ dumpValue f
    return f

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget
    -- First run standard code.
    m <- newModule
    iovec <- defineModule m cgvec

    fptr <- runEngineAccess $ do addModule m; getPointerToFunction iovec
    let fvec = convert fptr

    fvec 10 >>= print

    vec <- runEngineAccess $ do addModule m; generateFunction iovec

    vec 10 >>= print

    -- And then optimize and run.
    _ <- optimizeModule 1 m

    funcs <- getModuleValues m
    dumpFunction iovec
    -- print $ map fst funcs

    let iovec' :: Function 'C (T -> IO T)
        Just iovec' = castModuleFunction =<< lookup "vectest" funcs
        ioretacc' :: Function 'C (IO T)
        Just ioretacc' = castModuleFunction =<< lookup "retacc" funcs

    (vec', retacc') <- runEngineAccess $ do
        addModule m
        liftM2 (,) (generateFunction iovec') (generateFunction ioretacc')

    dumpFunction iovec'

    vec' 10 >>= print
    vec' 0 >>= print
    retacc' >>= print
