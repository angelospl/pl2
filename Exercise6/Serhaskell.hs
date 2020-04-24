import Serialcore
---------------------------serial--------------------------
calc_results::[(Integer,Integer,Integer)]->[Integer]
calc_results xs = map simple_calc xs

main:: IO ()
main = do
  [t] <- readInts
  lista <- read_input t []
  --serial
  mapM_ print (calc_results (transform_res lista))
