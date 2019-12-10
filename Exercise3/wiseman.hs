module Main where
import Data.Array
import Data.Char

wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

helper::Integer->Integer
helper 0=1
helper 1=1
helper n=
  let streaklist= takeWhile (\x -> x <= n) wins in
  (sum [(fast_helper !(n-streakpoints)) `mod` 1000000007 | streakpoints <-wins, streakpoints<=n]) `mod` 1000000007

fast_helper= listArray (0,n-1) $ map helper [0..n]
  where n=1000000

solution::Integer->Integer
solution 0=1
solution n=2*(helper n)

solution_diff::Integer->Integer->Integer
solution_diff a b=
  sum [(solution x) `mod` 1000000007 | x <- [a..b]] `mod` 1000000007


main :: IO ()
main = do
  print $ solution_diff 500000 999999
