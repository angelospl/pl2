module Main where
import Data.Array
import Data.Char
import Control.Monad.Cont


wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution::Integer->Integer->Integer
solution 0 _=1
solution n m=2*(helper m n)

helper::Integer->Integer->Integer
helper _ 0=1
helper _ 1=1
helper m n=
  let streaklist= takeWhile (\x -> x <= n) wins in
  (sum [(score_array !(n-streakpoints)) `mod` m | streakpoints <-streaklist]) `mod` m
    where score_array = fast_helper m

fast_helper::Integer->Array Integer Integer
fast_helper m= listArray (0,n-1) $ map (helper m) [0..n]
        where n=1000000


solution_diff::(Integer,Integer,Integer)->Integer
solution_diff (a,b,m) =
  (sum [(solution x $! m) `mod`  m | x <- [a..b]]) `mod` m

readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
  forM_ [1..n] $ \i -> do
    [a,b] <- readInts
    print $ solution_diff $!(a,b,m)
