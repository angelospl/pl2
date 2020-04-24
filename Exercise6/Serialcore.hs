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

transform_res::[Integer]->[(Integer,Integer,Integer)]
transform_res [] = []
transform_res (n:k:p:xs) = (n,k,p):transform_res xs


gcdExtended::Integer->Integer->Integer->Integer->(Integer,Integer,Integer,Integer)
gcdExtended a b x y =
  if a == 0 then (a,b,0,1)
  else (a,b,x1,y1) where
    (_,_,retx,rety) = gcdExtended (b `mod` a) a x y
    x1 = rety - b `div` a * retx
    y1 = retx

inverse::Integer->Integer->Integer
inverse x p =
  let (_,_,ret,_) = gcdExtended x p 0 0 in
    if ret < 0 then ret + p
      else ret

modMult::Integer->Integer->Integer->Integer
modMult p a b= (a*b) `mod` p

modFact::Integer->Integer->Integer->Integer
modFact n k p =
  foldl' (modMult p) 1 [k+1..n]

simple_calc::(Integer,Integer,Integer)->Integer
simple_calc (n,k,p) =
  modMult p nk invnminusk where
    nk = modFact n k p
    invnminusk = inverse (modFact (n-k) 1 p) p
