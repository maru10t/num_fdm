module Main where

import Lib
import Data.Array.Repa as R
import System.IO
import Debug.Trace
import Text.Printf

type Field t = Array t DIM1 Double
type Field2D t = Array t DIM2 Double

nx = 20 :: Int
ny = 20 :: Int
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
--            hPutStrLn fileh $ trace str str
            hPutStrLn fileh str
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
        u' <- computeP $ step $ u
--        let fname = printf "data/output%03d.txt" (100-n)
--        writeField fname u'
        stepField2D (n-1) u' step

step02D :: Field2D U -> Field2D D
step02D u = fromFunction (Z:.nx:.ny) (\(Z:.i:.j) -> 1/4 * (a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1)) )
  where a (i,j)
          | isOut(i,j) = 0.0
          | otherwise  = u!(Z:.i:.j)

isBound :: (Int,Int) -> Bool
isBound (i,j) = (i==0)||(j==0)||(i==(nx-1))||(j==(ny-1))

isOut :: (Int,Int) -> Bool
isOut (i,j) = (i<0) || (j<0) || (i>=nx) || (j>=ny)

-- boundary condition (1-1)
bcond11 :: Field2D D -> Field2D D
bcond11 u = fromFunction (Z:.nx:.ny) f
                where f (Z:.i:.j)
                        | isBound(i,j)  = (fromIntegral j) * dy
                        | otherwise     = u!(Z:.i:.j)

bcond12 :: Field2D D -> Field2D D
bcond12 u = fromFunction (Z:.nx:.ny) f
                where f (Z:.i:.j)
                        | isBound(i,j)  = -(fromIntegral i) * dx
                        | otherwise     = u!(Z:.i:.j)

bcond13 :: Field2D D -> Field2D D
bcond13 u = fromFunction (Z:.nx:.ny) f
                where f (Z:.i:.j)
                        | isBound(i,j)  = 1.0
                        | otherwise     = u!(Z:.i:.j)

initCond12D :: Field2D U
initCond12D = computeS $ fromFunction (Z:.nx:.ny) (\(Z:.i:.j) -> 0.0)

liftD2U :: (Field2D D -> Field2D D) -> (Field2D U -> Field2D U)
liftD2U f = computeS.f.delay

main :: IO ()
main = do
  u1 <- stepField2D 100 (liftD2U bcond11 $initCond12D) (bcond11.step02D)
  writeField "data/cond11.txt" u1
  u2 <- stepField2D 100 (liftD2U bcond12 $initCond12D) (bcond12.step02D)
  writeField "data/cond12.txt" u2
  u3 <- stepField2D 100 (liftD2U bcond13 $initCond12D) (bcond13.step02D)
  writeField "data/cond13.txt" u3
  return ()
