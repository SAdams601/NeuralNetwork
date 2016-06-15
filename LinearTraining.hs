module LinearTraining where
import Perceptron
import System.IO.Unsafe
import System.Random

newtype Line = Line (Int,Int)

unsafeRandomLine :: Line
unsafeRandomLine = Line (a,b)
  where a = unsafePerformIO $ randomRIO (1,10)
        b = unsafePerformIO $ randomRIO (1,10)

f :: Line -> Float -> Float
f (Line (a,b)) x = ((fromIntegral a) * x) + (fromIntegral b)

isAboveLine :: [Float] -> Line -> Int
isAboveLine [x,y] ln = if y > f ln x
                       then 1
                       else 0

unsafeGetPoint :: [Float]
unsafeGetPoint = take 2 (randomRs (1,100) (unsafePerformIO getStdGen))

trainPerceptron :: Perceptron -> Int -> Float -> Perceptron
trainPerceptron per iters lr = undefined

iteration :: Line -> Perceptron -> Float -> Perceptron
iteration ln per lr = let point = unsafeGetPoint
                          actual = run per point
                          expected = isAboveLine point ln
                          delta = expected - actual
                      in
                      adjust per point (fromIntegral delta) lr
