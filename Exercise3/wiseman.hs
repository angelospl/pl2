module Main where
import Data.Array
import Data.Char
import Control.Monad.Cont
import Control.Monad.Fix

wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution_diff::(Integer,Integer,Integer)->Array Integer Integer->Integer
solution_diff (a,b,m) array=
   (sum [(solution x) `mod`  m | x <- [a..b]]) `mod` m
    where solution 0=1
          solution x=2*(array!x)

create_array::Integer->Integer->Array Integer Integer
create_array m maxN= ret
  where ret= array (0,maxN) ((0,1):(1,1):[(i,helper i)| i<-[2..maxN]])
        helper n=
            let streaklist= takeWhile (\x -> x <= n) wins in
            (sum [(ret !(n-streakpoints)) `mod` m | streakpoints <-streaklist]) `mod` m


read_input::Integer->[Integer]->IO [Integer]
read_input n lista =
  if n==0 then do return lista
  else do
    x<- readInts
    read_input (n-1) (lista++x)


print_results::[Integer]->Integer->Array Integer Integer-> IO ()
print_results [a,b] m array= print $ solution_diff (a,b,m) array
print_results (a:b:xs) m array= do
  print $ solution_diff (a,b,m) array
  print_results xs m array

readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
  lista <- read_input n []
  print_results lista m (create_array m (maximum(lista)))
