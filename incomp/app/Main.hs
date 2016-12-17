module Main where

import Lib
import Data.Array.Repa as R
import System.IO
import Debug.Trace

type Field t = Array t DIM1 Double
type Field2D t = Array t DIM2 Double

nx = 10 :: Int
ny = 10 :: Int
dx = 1.0 / (fromIntegral nx) :: Double
dy = 1.0 / (fromIntegral ny) :: Double
dt = 2.0 / 1000 :: Double

stepField :: Int -> Field U -> (Field U -> Field D) -> IO (Field U)
stepField n u step = do
  putStrLn $ reverse.tail.reverse.tail $ show . toList $ u
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

mkXarr :: Field2D D
mkXarr = fromFunction (Z:.nx:.ny) (\(Z:.i:.j) -> (fromIntegral (nx*i)))

mkYarr :: Field2D D
mkYarr = fromFunction (Z:.nx:.ny) (\(Z:.i:.j) -> (fromIntegral (ny*j)))

writeField :: FilePath -> Field2D U -> IO ()
writeField path u = do
  let loop fileh j =
        if (j == ny)
          then do return ()
          else do
            let line = slice u (Any:.(j::Int))
            let str = reverse.tail.reverse.tail $ show.toList $ line
            hPutStrLn fileh $ trace str str
            loop fileh (j+1)

  fh <- openFile path WriteMode
  loop fh 0
  hClose fh

stepField2D :: Int -> Field2D U -> (Field2D U -> Field2D D) -> IO (Field2D U)
stepField2D n u step = do
--  putStrLn $ reverse.tail.reverse.tail $ show . toList $ u
  if (n == 0)
    then (return u)
    else do
        u' <- computeP $ appBound . step $ u
        stepField2D (n-1) u' step

isBound :: (Int,Int) -> Bool
isBound (i,j) = (i==0)||(j==0)||(i==(nx-1))||(j==(ny-1))

step02D :: Field2D U -> Field2D D
step02D u = fromFunction (Z:.nx:.ny) (\(Z:.i:.j) -> 1/4 * (a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1)) )
  where a (i,j)
          | isOut(i,j) = 0
          | otherwise  = u!(Z:.i:.j)
        isOut (i,j) = (i<0) || (j>0) || (i>=nx) || (j>=ny)

appBound :: Field2D D -> Field2D D
appBound u = fromFunction (Z:.nx:.ny) f
                where f (Z:.i:.j)
                          | isBound(i,j) = 1.0
                          | otherwise    = u!(Z:.i:.j)

initCond02D :: Field2D U
initCond02D = computeS $ fromFunction (Z:.nx:.ny)
  (\(Z:.i:.j) ->
      let x = (fromIntegral i)*dx
          y = (fromIntegral j)*dy
      in if (x<0.5)
          then x
          else (1.0-x) )

initCond12D :: Field2D U
initCond12D = computeS $ fromFunction (Z:.nx:.ny) f
                where f (Z:.i:.j)
                          | i==0      = 1.0
                          | otherwise = 0.0

main :: IO ()
main = do
  u <- stepField2D 100 initCond12D step02D
  return ()
