module Main where


data Game = Win | Loss

instance Eq Game where
  (==) Win Win = True
  (==) Loss Loss = True
  (==) _ _ = False

--takes a game an integer that shows the previous score and returns the score
point_eval::Game -> Integer -> Integer
point_eval game prev= if (game == Win)
                      then if prev > 0
                           then 2*prev
                           else 1
                      else if prev > 0
                           then 0
                           else -1

--a function that takes a list and returns the points
points::[Game]-> Integer -> Integer-> Integer
points [] _ acc=acc
points (game:tl) prevs acc= let newpoints=point_eval game prevs in
                            if newpoints == -1 then -1
                            else points tl newpoints (acc+newpoints)

first::(a,b,c)->a
first(x,_,_)=x

second::(a,b,c)->b
second(_,x,_)=x

third::(a,b,c)->c
third(_,_,x)=x


memoize::(Int->Int->a)->[[a]]
memoize f=map (\x-> map (f x) [0..]) [0..]

dp::Int->Int->(Int,Int,Int)
dp 0 0=(0,1,0)
dp 0 1=(0,1,0)
dp 0 2=(0,0,1)
dp i 0 =(0,0,0)
dp 0 j =(0,0,0)
dp 1 1 =(1,0,0)
dp 1 2 =(1,1,0)
dp i j=((wins ((floor.logBase 2 .fromIntegral)(i+1)) 0 i j),first(dp i (j-1)),second (dp i (j-1)))
  where wins 0 acc _ _=acc
        wins winstreak acc i j
          | i<0 || j<0 = acc
          | otherwise = wins (winstreak-1) (acc+second(dp (i-(2^winstreak-1)) (j-winstreak))) i j


dpstore::[[(Int,Int,Int)]]
dpstore=memoize dp

fastdp::Int->Int->(Int,Int,Int)
fastdp x y=dpstore !! x !! y



main :: IO ()
main =
  putStrLn $ show (dp 37 37)
