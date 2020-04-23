import Control.Parallel.Strategies
import Control.Parallel
import Control.Concurrent
import Data.List
import Serialcore

---------------------------concurrent--------------------------
calcThread::MVar Integer -> (Integer,Integer,Integer) -> IO ()
calcThread resultMVar nkp  =
  do
    pseq f (return  ())
    putMVar resultMVar f
  where
    f = simple_calc nkp

conc_calc::(Integer,Integer,Integer) -> IO (MVar Integer)
conc_calc x = do
  resMVar <-newEmptyMVar
  forkIO (calcThread resMVar x)
  return resMVar

conc_sum::[(Integer,Integer,Integer)] -> IO ([MVar Integer])
conc_sum lst = mapM conc_calc lst


main:: IO ()
main = do
  [t] <- readInts
  lista <- read_input t []
  --concurrent
  retlst <- conc_sum (transform_res lista)
  retlst2 <- mapM takeMVar retlst
  mapM_ print retlst2
