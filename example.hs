
import Hawkes
import Control.Monad.Random
import Graphics.Gnuplot.Simple


laste evt =
	case (children evt) of
		[] -> time evt
		otherwise -> laste $! last $ children evt

main :: IO ()
main = do      
  let mu     = 2
      alpha  = 9
      beta   = 10
      event = evalRand (simulate mu alpha beta 10000)  (mkStdGen 14) 
      ts     = flatten event

  --xOpt <- estimate ts 4 3 12
  --plotList [] ts
  print $ length ts
  --print xOpt