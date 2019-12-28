module Main where
import Data.Array
import Data.Char
import Control.Monad.Cont


wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

solution_diff::([Integer],Integer)->Integer
solution_diff (lista,m) =
  (sum [(solution $! x) `mod`  m | x <-lista ]) `mod` m
  where solution 0=1
        solution n=2*(fast_helper ! n)
          where fast_helper=listArray (0,n-1) $ map helper [0..n]
                  where n=1000000
                        helper 0=1
                        helper 1=1
                        helper n=
                          let streaklist= takeWhile (\x -> x <= n) wins in
                          (sum [(fast_helper !(n-streakpoints)) `mod` m | streakpoints <-streaklist]) `mod` m



readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
  forM_ [1..n] $ \i -> do
    [a,b] <- readInts
    print $ solution_diff $!([a..b],m)
