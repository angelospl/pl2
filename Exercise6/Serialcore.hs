module Serialcore where
import Data.List
import Math.Combinatorics.Exact.Binomial

readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

read_input::Integer->[Integer]->IO [Integer]
read_input n lista =
  if n==0 then do return lista
  else do
    x<- readInts
    read_input (n-1) (lista++x)

calc_results::[(Integer,Integer,Integer)]->[Integer]
calc_results xs = map simple_calc xs

simple_calc::(Integer,Integer,Integer)->Integer
simple_calc (n,k,p) = (n `choose` k) `mod` p

transform_res::[Integer]->[(Integer,Integer,Integer)]
transform_res [] = []
transform_res (n:k:p:xs) = (n,k,p):transform_res xs
