module Main where

import Lib
import Data.Array.Repa as R
import System.IO
import System.Environment
import Debug.Trace
import Text.Printf
import Control.Monad.State

type Field t = Array t DIM1 Double
type Field2D t = Array t DIM2 Double

type Sf a = State (Fields D) a

type StreamFunc t = Field2D t
type Vorticity  t = Field2D t
type Fields t = (StreamFunc t, Vorticity t)

nx = 50 :: Int
ny = 50 :: Int
--re = 10
dx = 1.0 / (fromIntegral nx) :: Double
dy = 1.0 / (fromIntegral ny) :: Double

zeros_2d :: Field2D D
zeros_2d = fromFunction (Z:.nx:.ny) (\_ -> 0.0)

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

-- psi-omega
psi_omega_method :: Double -> Int -> Fields U -> IO (Fields U)
psi_omega_method re n u@(psi,om) = do
  if (n==0)
    then (return u)
    else do
      let (psi',om') = psi_omega_step re (bidelay u)
      u' <- bicomputeP (psi', om')
      psi_omega_method re (n-1) u'

psi_omega_step :: Double -> Fields D -> Fields D
psi_omega_step re (psi,om) 
  = execState proc (psi,om)
  where proc = do
                appBdCond_om
                step_om re
                step_psi re

-- apply Boundary condtions
appBdCond_om :: Sf ()
appBdCond_om = state (\(psi, om) -> 
    let om' = fromFunction (Z:.nx:.ny) f
        h = dx
        f (Z:.i:.j)
          | i==0        = -2*psi!(Z:.(i+1):.j)/(h**2)
          | i==(nx-1)   = -2*psi!(Z:.(i-1):.j)/(h**2)
          | j==0        = -2*psi!(Z:.i:.(j+1))/(h**2)
          | j==(ny-1)   = -2*(psi!(Z:.i:.(j-1)) + h)/(h**2)
          | otherwise   = om!(Z:.i:.j)
    in ((), (psi,om')))

step_om :: Double -> Sf ()
step_om re = state (\(psi,om) ->
  let om' = fromFunction (Z:.nx:.ny) f
      h = dx
      o(i,j) = om!(Z:.i:.j)
      p(i,j) = psi!(Z:.i:.j)
      f (Z:.i:.j)
        | isBound(i,j)  = o(i,j)
        | otherwise     = 1/4*(o(i+1,j)+o(i-1,j)+o(i,j+1)+o(i,j-1)) + re/16 * ((p(i,j+1)-p(i,j-1))*(o(i+1,j)-o(i-1,j)) - (p(i+1,j)-p(i-1,j))*(o(i,j+1)-o(i,j-1)))
    in ((), (psi,om')))

step_psi :: Double -> Sf ()
step_psi re = state (\(psi,om) ->
  let psi' = fromFunction (Z:.nx:.ny) f
      h = dx
      o(i,j) = om!(Z:.i:.j)
      p(i,j) = psi!(Z:.i:.j)
      f (Z:.i:.j)
        | isBound(i,j)  = 0.0 -- p(i,j)
        | otherwise     = h**2/4 * o(i,j) + 1/4 * (p(i+1,j)+p(i-1,j)+p(i,j+1)+p(i,j-1))
    in ((), (psi',om)))

initCond :: Fields D
initCond = (zeros_2d, zeros_2d)

bidelay :: Fields U -> Fields D
bidelay (u,v) = (delay u, delay v)

bicomputeP :: Fields D -> IO (Fields U)
bicomputeP (u,v) = do
  u' <- computeP u
  v' <- computeP v
  return (u', v')

init_st' :: Sf ()
init_st' = do
  put (zeros_2d,zeros_2d)

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

main :: IO ()
main = do
  args <- getArgs
  u0 <- bicomputeP initCond
  u@(psi,om) <- psi_omega_method (read $ args!!1) (read $args!!0) u0
  writeField "psi.txt" psi
