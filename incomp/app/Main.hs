module Main where

import Lib
import Data.Array.Repa as R

type Field t = Array t DIM1 Double
nx = 10 :: Int
dx = 1.0 / (fromIntegral nx) :: Double
dt = 2.0 / 1000 :: Double

stepField :: Int -> Field U -> (Field U -> Field D) -> IO (Field U)
stepField n u step = do
  if (n == 0)
    then (return u)
    else do
        u' <- computeP $ step u
        stepField (n-1) u' step

initCond0 :: Field U
initCond0 = fromListUnboxed (Z:.nx) [1.0..(fromIntegral nx)]

initCond1 :: Field U
initCond1 = computeS $ fromFunction (Z:.nx)
  (\(Z:.i) ->
      let x = (fromIntegral i)*dx
      in if (x<0.5)
          then x
          else (1.0-x) )

-- FTCS method
step0 :: Field U -> Field D
step0 u = fromFunction (Z:.nx) (\(Z:.i) -> r*a(i-1) + (1.0-2.0*r)*a(i) + r*a(i+1))
  where a j
          |j < 0 = 0.0
          |j >= nx = 0.0
          |otherwise = u!(Z:.j)
        r = dt/(dx**2)

run :: Int -> IO (Field U)
run n = do
  u <- stepField n initCond1 step0
  return u

main :: IO ()
main = do
  print "hello, world"
