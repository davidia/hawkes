{-# LANGUAGE BangPatterns #-}

import Numeric.AD.Newton
import Control.Monad.Random
import Data.List
import Graphics.Gnuplot.Simple
import Nlopt


opt n f = OptConfig { dim = n
                                  , xBounds = ([-1,-1,-1], replicate n 20)
                                  , xTolRel = 0
                                  , xTolAbs = replicate n 0.01
                                  , fTolRel = 0
                                  , fTolAbs = 0
                                  , algorithm = NLOPT_LN_COBYLA
                                  , objFun = f
                                  }

main :: IO ()
main = do    
  --values <- evalRandIO (dice 2)
  --values <- evalRandIO (simulate 0.001 0.8 1 500)
  -- 6 9 11
  let values = evalRand (simulate 2  11.5 12 10000)  (mkStdGen 9) 
      sim = sort $ merge 0 values
      xs = [3..20]
      ys = [5..15]
      f  = \x y -> (x,y,logLikelihood sim 5 x y)

  --let as = [-2,-1.8..2::Double] in plotMesh3d [] [] (do x <- as; return (do y <- as; return (x,y,cos(x*x+y*y))))
  -- plotMesh3d [] [] [ (eval f 1 1) ]
  --plotFunc3d [] [] xs xs (\x y -> logLikelihood sim x y 15)
  --putStrLn (show values)
  --print $ length sim
  _ <- optimize (opt 3 (logLikelihood' sim))
  --plotList [Title "mu"] $ map (\x -> logLikelihood sim x 10 12) xs
  --plotList [Title "alpha"] $ map (\x -> logLikelihood sim 5 x  12) xs
  --plotList [Title "beta"] $ map (\x -> logLikelihood sim 5 10 x) xs
  --print $ logLikelihood' sim [1.29,-5.33,-4.93]
  --print $ logLikelihood sim 2.29 (11-5.33) (12-4.93)
  --print $ logLikelihood sim 2.29 (11-5.33) (12-4.93)
  --print $ logLikelihood sim 5 10 15
  ----print $ logLikelihood' sim  [9.999996423721313e-2,0.1,0.1]
  --print $ logLikelihood' sim  [0 ,0, 0]

  --plotList [] $ sort $ extract values
  --putStrLn (show sim)
  --let x = evalRand (simulate 0.1 0.1 1 1) mkStdGen
  print 1 -- $ 
   
  --let s = take 1 $ gradientAscent (logLikelihood' (map lift sim)) [0.002, 1, 3]
  --print $ last s
  --print $ logLikelihood' sim (last s)


data Event = Event Double [Event]  deriving (Show)

logLikelihood' ts params = negate $ logLikelihood ts (params !! 0 + 1) (params !! 1 + 1) (params !! 2 + 1)

--  Loglikelihood fucntion for exponential hawkes process
--  ts - event times
--
--  Implements the following: 
--
--  -mu *t + sum a/b (exp(-beta(tn-ti))-1) + sum log (mu + a A(i))
--  A(i) = sum (j<i) exp(-beta(ti-tj))
--
logLikelihood ts mu alpha beta  = 
  -mu * tn + s1 + s2
  where tn   = last ts
        e ti = (alpha / beta) * ( exp( -beta * ( tn - ti ) ) - 1 )
        f x  = log(mu + alpha * x)
        s1   = sum $ map e ts
        s2   = sum $ map f (calcA ts beta)


eval f x y  
  | x > y = eval f 1 (y+1)
  | y > 20 = []
  | otherwise = f x y : eval f (x+1) y

      
calcA times beta = 0 : calcA' times beta 0
calcA' (t:[]) _    _   = []
calcA' (t:ts) beta acc = acc2 : calcA' ts beta acc2
  where acc2 = exp( -beta * d ) * (1 + acc)
        d    = head ts - t



extract :: Event -> [Double]
extract (Event t c)  = t : (foldl (\a b-> a ++ extract b) [] c )


-- collapse tree into event arrival times
merge :: Double -> Event -> [Double]
merge acc (Event t c)  = acc' : (foldl (\a b-> a ++ merge acc' b) [] c )
  where acc' = acc + t
        
-- simulate the hawkes process
simulate :: (RandomGen g) => Double -> Double ->Double ->Int -> Rand g Event
simulate mu alpha beta n = do
  waits <- sequence (replicate n $ wait mu)  
  let mothers = scanl1 (\a b -> a+b)  waits
  evts <- mapM (build alpha beta) mothers
  return $ Event 0 evts

-- sample poisson waiting time
wait lamda = do
  r <- getRandom  
  return $ -1/lamda*log(1-r)

-- contsruct an event given it's time and simulate it's chilldren alpha beta parameters
build a b t = do
  daughters <- spawn a b
  return $ Event t daughters

spawn ::  (RandomGen g) =>  Double -> Double -> Rand g [Event]
spawn alpha beta = do      
  sp   <- spawnCount alpha beta
  ts   <- sequence (replicate sp (spawnTime beta))
  e    <- mapM (build alpha beta) ts
  return $ e

spawnTime ::  (RandomGen g) => Double -> Rand g Double
spawnTime beta = do
  r <- getRandom
  return $ -log(1-r)/beta

spawnCount alpha beta = do 
  r <- getRandom
  return $ spawnCount' alpha beta r 0 0

spawnCount' alpha beta x acc n
  | acc' > x  = n
  | otherwise = spawnCount' alpha beta x acc' (n+1)
  where acc'    = acc + p
        l       = alpha/beta
        p       = exp(-l) * l^^n / fromIntegral(fact(n))

fact 0 = 1
fact n = n * fact (n-1)