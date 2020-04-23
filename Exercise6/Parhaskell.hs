import Control.Parallel.Strategies
import Control.Parallel
import Control.Concurrent
import Data.List
import Serialcore
---------------------------parallel--------------------------

par_calc_results::[(Integer,Integer,Integer)]->Eval [Integer]
par_calc_results xs= mapM (rpar . simple_calc) xs


main:: IO ()
main = do
  [t] <- readInts
  lista <- read_input t []
  --parallel
  mapM_ print (runEval (par_calc_results (transform_res lista)))
