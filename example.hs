
import Hawkes
import Control.Monad.Random

main :: IO ()
main = do      
  let mu     = 2
      alpha  = 8
      beta   = 9
      events = evalRand (simulate mu alpha beta 100)  (mkStdGen 9) 
      ts     = merge 0 events
  --(xOpt, fOpt, res) <- estimate ts 4 3 12
  print $ length ts