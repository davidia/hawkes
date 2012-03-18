
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
      alpha  = 8
      beta   = 9
      event = evalRand (simulate mu alpha beta 10000)  (mkStdGen 9) 
      ts     = flatten event

  (xOpt, fOpt, res) <- estimate ts 4 3 12
  --plotList [] ts
  print $ length xOpt