module Main (main) where

import Control.Monad.State.Lazy
import Data.Foldable
import Queue (Queue)
import qualified Queue
import Tapper (Sample (Sample), Tapper)
import qualified Tapper
import Test.HUnit

revrange :: Int -> [Int]
revrange n | n <= 0 = []
revrange n = n : revrange (n - 1)

range :: Int -> [Int]
range = reverse . revrange

-- TODO: icky

epsEqual :: Float -> Float -> Sample -> Bool
epsEqual d x (Sample y) = abs (x - y) <= d

eps :: Float
eps = 0.001

approxEqual :: Float -> Sample -> Bool
approxEqual = epsEqual eps

pushBpms :: [Float] -> Tapper -> Tapper
pushBpms xs t = foldl (flip Tapper.pushBpm) t (map Sample xs)

pushMany :: [a] -> Queue a -> Queue a
pushMany = flip (foldl (flip Queue.push))

makeStateTest :: s -> StateT s IO () -> Test
makeStateTest z = TestCase . flip evalStateT z

makeStateTestList :: s -> [(String, StateT s IO ())] -> Test
makeStateTestList z =
  TestList . map (\(l, s) -> TestLabel l (makeStateTest z s))

pushPop :: StateT (Queue Int) IO ()
pushPop = do
  put (Queue.create 8)
  modify (pushMany (range 8))
  gets toList >>= lift . assertEqual "push works" [1, 2, 3, 4, 5, 6, 7, 8]
  gets Queue.isFull >>= lift . assertBool "full"
  state Queue.pop >>= lift . assertEqual "1st pop" (Just 1)
  state Queue.pop >>= lift . assertEqual "2nd pop" (Just 2)
  state Queue.pop >>= lift . assertEqual "3rd pop" (Just 3)
  gets toList >>= lift . assertEqual "pop works" [4, 5, 6, 7, 8]

pushClobber :: StateT (Queue Int) IO ()
pushClobber = do
  put (Queue.create 8)
  modify (pushMany (range 8))
  gets toList >>= lift . assertEqual "push works" [1, 2, 3, 4, 5, 6, 7, 8]
  modify (Queue.push 24)
  gets toList >>= lift . assertEqual "push clobbers oldest element when full" [2, 3, 4, 5, 6, 7, 8, 24]

clear :: StateT (Queue Int) IO ()
clear = do
  put (Queue.create 8)
  modify (\q -> foldl pushThreePopOne q (range 8))
  gets (not . Queue.isEmpty) >>= lift . assertBool "not empty"
  modify Queue.clear
  gets Queue.isEmpty >>= lift . assertBool "now is empty"
  gets toList >>= lift . assertEqual "empty contents" []
  where
    pushThreePopOne :: Queue Int -> Int -> Queue Int
    pushThreePopOne q i =
      let (_, q') =
            Queue.pop
              . Queue.push (i * 3)
              . Queue.push (i * 2)
              . Queue.push (i * 1)
              $ q
       in q'

truncateBack :: StateT (Queue Int) IO ()
truncateBack = do
  put (Queue.create 8)
  modify (\q -> foldl (flip Queue.push) q (range 8))
  modify (Queue.truncateBack 3)
  gets toList >>= lift . assertEqual "truncated down to 3" [6, 7, 8]
  modify (Queue.truncateBack 100)
  gets toList >>= lift . assertEqual "truncate up is nop" [6, 7, 8]

display :: StateT Tapper IO ()
display = do
  put (Tapper.create 10 True)
  modify (pushBpms [120.051, 112.41, 121.105])
  gets show >>= lift . assertEqual "to string works" "[121.1, 112.4, 120.1]"
  modify Tapper.clear
  gets show >>= lift . assertEqual "empty to string" "[]"
  modify (Tapper.pushBpm (Sample 112.76))
  gets show >>= lift . assertEqual "to string with one element works" "[112.8]"

isRecording :: StateT Tapper IO ()
isRecording = do
  put (Tapper.create 10 True)
  gets (not . Tapper.isRecording) >>= lift . assertBool "starts not recording"
  get >>= lift . Tapper.tap >>= put
  gets Tapper.isRecording >>= lift . assertBool "records first tap"
  get >>= lift . Tapper.tap >>= put
  gets Tapper.isRecording >>= lift . assertBool "keeps recording taps"
  modify Tapper.clear
  gets (not . Tapper.isRecording) >>= lift . assertBool "stops once cleared"

bpm :: StateT Tapper IO ()
bpm = do
  put (Tapper.create 10 True)
  gets (approxEqual 0.0 . Tapper.bpm) >>= lift . assertBool "starts at 0.0 BPM"
  modify (Tapper.pushBpm (Sample 23.0))
  gets (approxEqual 23.0 . Tapper.bpm) >>= lift . assertBool "1st tap"
  modify (Tapper.pushBpm (Sample 26.0))
  gets (approxEqual 24.5 . Tapper.bpm) >>= lift . assertBool "2nd tap"
  modify (Tapper.pushBpm (Sample 29.0))
  gets (approxEqual 26.0 . Tapper.bpm) >>= lift . assertBool "3rd tap"
  modify (Tapper.pushBpm (Sample 61.0))
  gets (approxEqual 34.75 . Tapper.bpm) >>= lift . assertBool "4th tap"

tap :: StateT Tapper IO ()
tap = do
  put (Tapper.create 3 True)
  gets Tapper.count >>= lift . assertEqual "starts empty" 0
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "1st tap, no span yet" 0
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "2nd tap" 1
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "3rd tap" 2
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "4th tap" 3
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "5th, oldest dropped" 3

  modify Tapper.toggleBounded
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "1st tap after unbounding, can hold more" 4
  get >>= lift . Tapper.tap >>= put
  gets Tapper.count >>= lift . assertEqual "2nd tap after unbounding" 5
  modify Tapper.toggleBounded
  gets Tapper.isBounded >>= lift . assertBool "is now bounded"
  gets Tapper.count >>= lift . assertEqual "truncated after bounding" 3

tapperTruncate :: StateT Tapper IO ()
tapperTruncate = do
  put (Tapper.create 3 True)
  modify (pushBpms [80.0, 70.0, 60.0])
  gets show >>= lift . assertEqual "initialized" "[60.0, 70.0, 80.0]"

  modify (Tapper.pushBpm (Sample 50.0))
  gets show >>= lift . assertEqual "push evicts oldest sample" "[50.0, 60.0, 70.0]"

  modify Tapper.toggleBounded
  modify (Tapper.pushBpm (Sample 40.0))
  gets show >>= lift . assertEqual "nothing evicted from unbounded buffer" "[40.0, 50.0, 60.0, 70.0]"

  modify Tapper.toggleBounded
  gets show >>= lift . assertEqual "after bounding, oldest evicted" "[40.0, 50.0, 60.0]"

resize :: StateT Tapper IO ()
resize = do
  put (Tapper.create 3 True)
  modify (pushBpms [80.0, 70.0, 60.0])
  gets show >>= lift . assertEqual "initialized" "[60.0, 70.0, 80.0]"

  modify (Tapper.resize 2)
  gets show >>= lift . assertEqual "after resize, oldest evicted" "[60.0, 70.0]"
  gets Tapper.capacity >>= lift . assertEqual "capacity is smaller now" 2

queueTests :: Test
queueTests =
  makeStateTestList
    (Queue.create 0)
    [ ("push pop", pushPop),
      ("push clobber", pushClobber),
      ("clear", clear),
      ("truncate back", truncateBack)
    ]

tapperTests :: Test
tapperTests =
  makeStateTestList
    (Tapper.create 0 True)
    [ ("display", display),
      ("is recording", isRecording),
      ("bpm", bpm),
      ("tap", tap),
      ("truncate", tapperTruncate),
      ("resize", resize)
    ]

tests :: Test
tests = TestList [TestLabel "Queue" queueTests, TestLabel "Tapper" tapperTests]

main :: IO ()
main = runTestTTAndExit tests
