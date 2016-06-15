module Perceptron where
import System.Random
import System.IO.Unsafe
import Control.Monad

data Perceptron = Per{ actF :: (Float -> Int), weights :: [Float], bias :: Float}

heaviside :: Float -> Int
heaviside f = if f < 0
              then 0
              else 1

basicPerceptron :: Int -> Perceptron
basicPerceptron i = Per{actF = heaviside, weights = weights, bias = 1}
  where weights = unsafePerformIO $ replicateM i randomIO 

run :: Perceptron -> [Float] -> Int
run per inputs = let sum = foldl (+) 0 (zipWith (*) inputs (weights per)) in
  (actF per) $ sum + (bias per)

adjust :: Perceptron -> [Float] -> Float -> Float -> Perceptron
adjust per inputs delta lr = per {weights = newWeights, bias = newBias}
  where newBias = (bias per) + delta * lr
        newWeights = zipWith (+) (weights per) (map (lr*delta*) inputs)  
