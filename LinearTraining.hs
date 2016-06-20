module LinearTraining where
import Perceptron
import System.IO.Unsafe
import System.Random
import Control.Monad

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

trainPerceptron :: Perceptron -> Int -> Line -> Float -> Perceptron
trainPerceptron per iters ln lr = if iters == 0
                                  then per
                                  else let newPer = iteration ln per lr in
                                  trainPerceptron newPer (iters-1) ln lr

iteration :: Line -> Perceptron -> Float -> Perceptron
iteration ln per lr = let point = unsafeGetPoint
                          actual = run per point
                          expected = isAboveLine point ln
                          delta = expected - actual
                      in
                      adjust per point (fromIntegral delta) lr

verify :: Line -> Perceptron -> IO ()
verify ln per = do
  let results = replicate 100 verifyPer
      correct = length (filter (\r -> r) results)
  putStrLn $ "The number of correct results is: " ++ (show correct)
    where verifyPer = let pt = unsafeGetPoint
                          act = run per pt
                          expected = isAboveLine pt ln in
                      act == expected
  

main :: IO ()
main = do
  let per = basicPerceptron 2
      line = unsafeRandomLine
  putStrLn $ "The perceptron before training:\n" ++ (show per) 
  putStrLn "Verifying the perceptron before training"
  verify line per
  let trained = trainPerceptron per 1000 line 0.1
  putStrLn $ "The perceptron after training:\n" ++ (show trained)
  putStrLn "Verifying the perceptron after training"
  verify line trained
