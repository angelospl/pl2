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


main :: IO ()
main = putStrLn "Hello World"
