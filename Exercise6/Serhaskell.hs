import Serialcore
---------------------------serial--------------------------

main:: IO ()
main = do
  [t] <- readInts
  lista <- read_input t []
  --serial
  mapM_ print (calc_results (transform_res lista))
