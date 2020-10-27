{-# OPTIONS_GHC -O #-}

import Data.Bits (shiftR)
import Data.List

-- the basic idea of the solution is the following :
--                               indexes :     0   1   2   3   4   5   6
-- Imagine we have the list with the results [ 2 , 2 , 2 , 4 , 6 , 8 , 12..]
-- each element i is created by summing x previous elements that are found in positions i - k, where x = floor (logbase 2 i) and k = 2^e-1 for e = 1..x
-- therefore the element 3 needs the 2nd(3 - (2^1 -1)) element as well as the 0th(3 - (2^2 -1)) element or  the element 7 needs the 0th,4th,and 6th element
-- those are the dependencies for each element. In order to solve those dependencies we can consider that we have calculated the above list of results
-- and we can take sublists from this list with the elements needed as shown below
-- id 0   1   2   3   4   5   6    7    8
--  [ 2 , 2 , 2 , 4 , 6 , 8 , 12 , 20 , 30 ..]
--  [ 0   2   2   2   4   6   8    12   20 ..]
--  [         0   2   2   2   4    6    8 ..]
--  [                         0    2    2  ..]
--
-- Based on the above we have created the sublists needed. Those sublists start from the start of the initial list and have the same length as the initial
-- In order to match the length we add 0's at the start of each list. The first list is the list of all -1 previous, the second is the list of all -3,
-- the third of all the -7 previous etc. The amount of lists created is fixed by the maxn number. The reason why we can take sublists of the calculated results
-- before of their calculation is the Haskell's lazy evaluation, because when they are needed they are calculated.

create_helper_lists::Integer->Integer->[Integer]->[[Integer]]
create_helper_lists maxn ex lst=
  helper ex (2^ex) lst
    where helper 0 _ _ = []
          helper ex pow lst=
            let pow_i = fromIntegral pow
                maxn_i = fromIntegral maxn
                x = (take (pow_i-2) (repeat 0))++(take ( maxn_i -(pow_i - 1)) lst)
            in
              x `seq` x : helper (ex-1) (pow_i `shiftR` 1) lst

add_mod::Integer->Integer->Integer->Integer
add_mod m a b = (a + b) `mod` m


add_helper_lists::[[Integer]]->Integer->[Integer]
add_helper_lists (x:xs) m=
    foldl' (zipWith (add_mod m) ) x xs


calculate_points::Integer->Integer->[Integer]
calculate_points maxn m=
  let ex = (floor(logBase 2 (fromIntegral maxn)))
      lst = [2]++(add_helper_lists (create_helper_lists maxn ex lst) m)
  in
    lst


print_sums::[Integer]->Integer->[Integer]->IO ()
print_sums [] m pts = return ()
print_sums (a:b:xs) m pts = do
    let b_i = fromIntegral b
    let a_i = fromIntegral a
    let x = take (b_i+1) pts
    let y = drop a_i x
    print $ (foldl' (add_mod m) 0 y)
    print_sums xs m pts


read_input::Integer->[Integer]->IO [Integer]
read_input n lista =
  if n==0 then do return lista
  else do
    x <- readInts
    read_input (n-1) (lista++x)



readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

main :: IO ()
main = do
  [n,m] <- readInts
  lista <- read_input n []
  let maxm = maximum lista
  let (h:pts) = (calculate_points (maxm+1) m)
  let points = [1]++pts
  print_sums lista m points
