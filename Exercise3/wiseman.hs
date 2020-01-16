module Main where
import Data.Array
import Data.Char
import Control.Monad.Cont
import Control.Monad.Fix

wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution::Integer->Integer->Integer
solution 0 _=1
solution m n=2*(superfast_helper m n)

helper::Integer->(Integer->Integer)->Integer->Integer
helper _ _ 0=1
helper _ _ 1=1
helper m f n=
  let streaklist= takeWhile (\x -> x <= n) wins in
  (sum [(f (n-streakpoints)) `mod` m| streakpoints <-streaklist]) `mod` m


fast_helper::(Integer->Integer)->(Integer->Integer)
fast_helper f=(\x -> list ! x)
                where list= listArray (0,n-1) $ map f $! [0..n]
                            where n=1000000

superfast_helper::Integer->(Integer->Integer)
superfast_helper m=fix (fast_helper . (helper m))

solution_diff::(Integer,Integer,Integer)->Integer
solution_diff (a,b,m) =
  (sum [(solution m x) `mod`  m | x <- [a..b]]) `mod` m

readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

read_input::Integer->[Integer]->IO [Integer]
read_input n lista = do
  x <- readInts
  if n==1 then return lista
  else read_input (n-1) (x++lista)

print_results::[Integer]->Integer-> IO ()
print_results [] _= putStr ""
print_results (a:b:xs) m= do
  print $ solution_diff (a,b,m)
  print_results xs m


main :: IO ()
main = do
  [n,m] <- readInts
  lista <- read_input n []
  print_results lista m
