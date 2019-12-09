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


dp_solution n=sum [third (dp n y)| y <-[1..(2^n)+2]]

wins =[2^i-1 | i <- [1..19]] --all points from wins in an infinite list

helper::Int->Integer
helper 0=1
helper 1=1
helper n=
  let streaklist= takeWhile (\x -> x <= n) wins in
  (sum [(fast_helper (n-streakpoints)) `mod` 2019 | streakpoints <-wins, streakpoints<=n]) `mod` 2019

fast_helper::Int->Integer
fast_helper= (map helper [0..] !!)

solution::Int->Integer
solution 0=1
solution n=2*(helper n)

solution_a_b::Int->Int->Integer->Integer
solution_a_b a b acc=
  if a==b then
    (acc `mod` 2019 + solution b `mod`2019) `mod` 2019
  else
    solution_a_b (a+1) b ((acc `mod` 2019+(solution a)`mod`2019) `mod` 2019)


main :: IO ()
main =
  putStrLn $ show (dp 37 37)
