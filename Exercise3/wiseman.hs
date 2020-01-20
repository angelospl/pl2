module Main where
import Data.Array.IO
import Data.Array.MArray
import Data.Array.IArray
import Data.Char
import Data.Int
import Control.Monad.Cont

wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution_diff::(Int64,Int64,Int64)->Array Int64 Int64->Int64
solution_diff (a,b,m) array=
   (sum [(solution x) `mod`  m | x <- [a..b]]) `mod` m
      where solution 0=1
            solution x= 2*(array!x)

create_array::Int64->Int64->IO (Array Int64 Int64)
create_array m maxN=do
  arr <- newArray (0,maxN) 0::IO (IOUArray Int64 Int64)
  writeArray arr 0 1
  forM_ [0..maxN] $ (\i -> do
     val <- readArray arr i
     let j_list = takeWhile (\x -> x + i <= maxN ) wins
     forM_ j_list $ (\j -> do
       prev <-readArray arr (j+i)
       writeArray arr (j+i) ((val `mod` m) + (prev `mod` m) `mod` m)))
  ret <- freeze arr
  return ret

read_input::Int64->[Int64]->IO [Int64]
read_input n lista =
  if n==0 then do return lista
  else do
    x<- readInts
    read_input (n-1) (lista++x)


print_results::[Int64]->Int64->Array Int64 Int64-> IO ()
print_results [a,b] m array= print $ solution_diff (a,b,m) array
print_results (a:b:xs) m array= do
  print $ solution_diff (a,b,m) array
  print_results xs m array

readInts :: IO [Int64]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
  lista <- read_input n []
  arr<-create_array m (maximum lista)
  print_results lista m arr
