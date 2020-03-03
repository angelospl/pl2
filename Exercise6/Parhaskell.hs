import Control.Parallel.Strategies
import Control.Concurrent
import Data.List

perms::Integer->Integer->Integer
perms n 0 = 1
perms 0 k = 0
perms n k = perms (n-1) (k-1) * (n `div` k)

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
simple_calc (n,k,p) = (perms n k) `mod` p

transform_res::[Integer]->[(Integer,Integer,Integer)]
transform_res [] = []
transform_res (n:k:p:xs) = (n,k,p):transform_res xs

par_calc_results::[(Integer,Integer,Integer)]->Eval [Integer]
par_calc_results xs= mapM (rpar . simple_calc) xs

conc_calc_result::MVar [Integer]->[(Integer,Integer,Integer)]->IO ()
conc_calc_result mvar x = do
  let res = calc_results x
  putMVar mvar res

chunk::[(Integer,Integer,Integer)]->[Integer]
chunk lst = do
  mvar <- newEmptyMVar
  forkIO (conc_calc_result mvar lst)
  takeMVar mvar
  return mvar

conc_calc_results::[(Integer,Integer,Integer)]->Int->[(Integer,Integer,Integer)]
conc_calc_results lst n = do
  lsts@(x:xs) <- splitAt ((length lst) `div` n) lst
  return x
  --map chunk lsts

main:: IO ()
main = do
  [t] <- readInts
  lista <- read_input t []
  --serial
  mapM_ print (calc_results (transform_res lista))
  --parallel
  -- mapM_ print (runEval (par_calc_results (transform_res lista)))
  --concurrent
  --print_mvars (conc_calc_results (transform_res lista))
