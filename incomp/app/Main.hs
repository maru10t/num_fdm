module Main where

import Lib
import Data.Array.Repa as R

type Field t = Array t DIM1 Double
loop_n = 100 :: Int

stepField :: Int -> Field U -> (Field U -> Field D) -> IO (Field U)
stepField n u step = do
  if (n == 0)
    then (return u)
    else do
        u' <- computeP $ step u
        stepField (n-1) u' step

lst :: Field U
lst = fromListUnboxed (Z:.100) [1.0..100.0]

main :: IO ()
main = do
  u <- stepField 10 lst (\x -> R.map (*2) x)
  print u
