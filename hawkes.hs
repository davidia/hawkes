{-# LANGUAGE BangPatterns #-}

module Hawkes  (simulate,flatten,time,children,estimate) where

import Control.Monad.Random
import Data.List
import Nlopt

--
-- Simulation
--

data Event = Event {
  time     :: {-# UNPACK #-} !Double,
  children :: ![Event]
} deriving (Show)

-- flatten Event into arrival times
flatten :: Event -> [Double]
flatten t = sort $ squish 0 t []
  where squish t (Event x ts) xs = (t + x) : foldr (squish (t + x)) xs ts
        
-- simulate the hawkes process with exponential intensity:
-- 
-- mu + sum [ alpha * exp(-beta(t-ti)) ]
-- 
-- creates a simulation with n mother events
--
simulate :: (RandomGen g) => Double -> Double -> Double -> Int -> Rand g Event
simulate mu alpha beta n = do
  waits <- sequence (replicate n $! wait mu)  
  let mothers = scanl1 (\a b -> a+b)  waits
  evts <- mapM (build alpha beta) mothers
  return $! Event 0 evts

-- sample poisson waiting time
wait lamda = do
  r <- getRandom  
  return $! -1/lamda*log(1-r)

-- contsruct an event given it's time and simulate it's children alpha beta parameters
build a b t = do
  daughters <- spawn a b
  return $! Event t daughters

-- simulate daughter events
spawn ::  (RandomGen g) =>  Double -> Double -> Rand g [Event]
spawn alpha beta = do      
  count  <- spawnCount alpha beta
  ts     <- sequence (replicate count (spawnTime beta))
  e      <- mapM (build alpha beta) ts
  return $! e

-- simulate daughter event appearance time
spawnTime ::  (RandomGen g) => Double -> Rand g Double
spawnTime beta = do
  r <- getRandom
  return $! -log(1-r)/beta

-- simulate number of daughter events
spawnCount a b = do 
  let sc r n
        | memoized_cdf (a/b) n > r = n
        | otherwise                = sc r (n+1)        
  r <- getRandom
  return $! sc r 0

memoized_cdf l =
   let cdf l (-1) = 0       
       cdf l n  = (cdf l (n-1)) + exp(-l) * l^^n / fromIntegral(memoized_fact(n))
   in  (map (cdf l) [0 ..] !!)

memoized_fact = 
  let fact 0 = 1
      fact n = n * memoized_fact (n-1)       
   in  (map fact [0 ..] !!)

----
---- Estimation
----

estimate ts mu alpha beta = do
  let logLikelihood' p = negate $ logLikelihood ts (p !! 0 + mu) (p !! 1 + alpha) (p !! 2 + beta)
  (c,f,r) <- optimize (opt [-mu,-alpha,-beta] 3 logLikelihood')  
  return $ zipWith (+) [mu,alpha,beta] c

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

calcA times beta = 0 : calcA' times beta 0
calcA' (t:[]) _    _   = []
calcA' (t:ts) beta acc = acc2 : calcA' ts beta acc2
  where acc2 = exp( -beta * d ) * (1 + acc)
        d    = head ts - t


-- Nlopt options
opt lb n f = OptConfig {
  dim = n
, xBounds   = (lb, replicate n 20)
, xTolRel   = 0
, xTolAbs   = replicate n 0.01
, fTolRel   = 0
, fTolAbs   = 0
, algorithm = NLOPT_LN_COBYLA
, objFun    = f
}


