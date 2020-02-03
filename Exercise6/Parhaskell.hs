import Control.Parallel.Strategies
import Control.Concurrent

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

-- conc_calc_result::MVar (Integer,(Integer,Integer,Integer))-> IO ()
-- conc_calc_result mvar = do
--   (_,x) <- readMVar mvar
--   let res = simple_calc x
--   putMVar mvar (res,x)

-- loop::[MVar (Integer,(Integer,Integer,Integer))]->IO ()
-- loop [] = return ()
-- loop (mvar:mvars)= do
--   forkIO (conc_calc_result mvar)
--   loop mvars

conc_calc_results::[(Integer,Integer,Integer)]->[MVar (Integer,Integer,Integer)]
conc_calc_results lst = do
  let mvars = [mvar | j <- lst, mvar <- do newMVar j]
  --loop mvars
  return mvars

--
-- print_mvars [] = return ()
-- print_mvars (x:xs) = do
--   ret <- readMVar x
--   print ret
--   print_mvars xs

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
