module Main where
import Data.Array
import Data.Char
import Control.Monad.Cont
import Control.Monad.Fix

wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution::Integer->Integer
solution 0=1
solution n=2*(superfast_helper $! n)

helper::(Integer->Integer)->Integer->Integer
helper _ 0=1
helper _ 1=1
helper f n=
  let streaklist= takeWhile (\x -> x <= n) wins in
  (sum [(f (n-streakpoints)) | streakpoints <-streaklist])


fast_helper::(Integer->Integer)->(Integer->Integer)
fast_helper f=(\x -> list ! x)
                where list= listArray (0,n-1) $ map f [0..n]
                            where n=1000000

superfast_helper::Integer->Integer
superfast_helper=fix (fast_helper . helper)

solution_diff::(Integer,Integer,Integer)->Integer
solution_diff (a,b,m) =
  (sum [(solution x) `mod`  m | x <- [a..b]]) `mod` m

readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
  forM_ [1..n] $ \i -> do
    [a,b] <- readInts
    print $ solution_diff $!(a,b,m)
