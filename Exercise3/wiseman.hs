module Main where
import Data.Array
import Data.Char
import Control.Monad.Cont


wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution_diff::([Integer],Integer)->Array Integer Integer->Integer
solution_diff (lista,m) array=
   (sum [(solution x array) `mod`  m | x <-lista ]) `mod` m

create_array::Integer->Array Integer Integer
create_array m= ret
  where ret= array (0,999999) ((0,1):(1,2):[(i,helper i)| i<-[2..999999]])
        helper n=
            let streaklist= takeWhile (\x -> x <= n) wins in
            (sum [(ret !(n-streakpoints)) `mod` m | streakpoints <-streaklist]) `mod` m


solution::Integer->Array Integer Integer
solution 0 _ _=1
solution n array=array!n



readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
