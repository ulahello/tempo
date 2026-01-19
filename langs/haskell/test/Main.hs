module Main (main) where

import Data.Foldable
import qualified Queue
import Tapper (Sample (Sample))
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

-- TODO: use state variable instead of q'''''''...

pushPop :: Test
pushPop =
  TestCase
    ( do
        let q = Queue.create 8
        let q' = foldl Queue.push q (range (Queue.capacity q))
        assertEqual "push works" [1, 2, 3, 4, 5, 6, 7, 8] (toList q')
        assertBool "full" (Queue.isFull q')

        let (q'', x) = Queue.pop q'
        assertEqual "1st pop" (Just 1) x

        let (q''', x') = Queue.pop q''
        assertEqual "2nd pop" (Just 2) x'

        let (q'''', x'') = Queue.pop q'''
        assertEqual "3rd pop" (Just 3) x''

        assertEqual "pop works" [4, 5, 6, 7, 8] (toList q'''')
    )

pushClobber :: Test
pushClobber =
  TestCase
    ( do
        let q = Queue.create 8
        let q' = foldl Queue.push q (range (Queue.capacity q))
        assertEqual "push works" [1, 2, 3, 4, 5, 6, 7, 8] (toList q')

        let q'' = Queue.push q' 24
        assertEqual "push clobbers oldest element when full" [2, 3, 4, 5, 6, 7, 8, 24] (toList q'')
    )

clear :: Test
clear =
  TestCase
    ( do
        let q = Queue.create 8
        let q' = foldl pushThreePopOne q (range (Queue.capacity q))
        assertBool "not empty" (not (Queue.isEmpty q'))
        let q'' = Queue.clear q'
        assertBool "now is empty" (Queue.isEmpty q'')
        assertEqual "empty contents" [] (toList q'')
    )
  where
    pushThreePopOne q i =
      let push' = flip Queue.push
          (q', _) =
            Queue.pop
              . push' (i * 3)
              . push' (i * 2)
              . push' (i * 1)
              $ q
       in q'

truncateBack :: Test
truncateBack =
  TestCase
    ( do
        let q = Queue.create 8
        let q' = foldl Queue.push q (range (Queue.capacity q))
        let q'' = Queue.truncateBack q' 3
        assertEqual "truncated down to 3" [6, 7, 8] (toList q'')
        let q''' = Queue.truncateBack q'' 100
        assertEqual "truncate up is nop" [6, 7, 8] (toList q''')
    )

display :: Test
display =
  TestCase
    ( do
        let t = Tapper.create 10 True
        let t' = foldl Tapper.pushBpm t (map Sample [120.051, 112.41, 121.105])
        assertEqual "to string works" "[121.1, 112.4, 120.1]" (show t')
        let t'' = Tapper.clear t'
        assertEqual "empty to string" "[]" (show t'')
        let t''' = Tapper.pushBpm t'' (Sample 112.76)
        assertEqual "to string with one element works" "[112.8]" (show t''')
    )

isRecording :: Test
isRecording =
  TestCase
    ( do
        let t = Tapper.create 10 True
        assertBool "starts not recording" (not (Tapper.isRecording t))
        t' <- Tapper.tap t
        assertBool "records first tap" (Tapper.isRecording t')
        t'' <- Tapper.tap t'
        assertBool "keeps recording taps" (Tapper.isRecording t'')
        let t''' = Tapper.clear t''
        assertBool "stops once cleared" (not (Tapper.isRecording t'''))
    )

bpm :: Test
bpm =
  TestCase
    ( do
        let t = Tapper.create 10 True
        assertBool "starts at 0.0 BPM" (approxEqual 0.0 (Tapper.bpm t))
        let t' = Tapper.pushBpm t (Sample 23.0)
        assertBool "1st tap" (approxEqual 23.0 (Tapper.bpm t'))
        let t'' = Tapper.pushBpm t' (Sample 26.0)
        assertBool "2nd tap" (approxEqual 24.5 (Tapper.bpm t''))
        let t''' = Tapper.pushBpm t'' (Sample 29.0)
        assertBool "3rd tap" (approxEqual 26.0 (Tapper.bpm t'''))
        let t'''' = Tapper.pushBpm t''' (Sample 61.0)
        assertBool "4th tap" (approxEqual 34.75 (Tapper.bpm t''''))
    )

tap :: Test
tap =
  TestCase
    ( do
        let t = Tapper.create 3 True
        assertEqual "starts empty" 0 (Tapper.count t)
        t' <- Tapper.tap t
        assertEqual "1st tap, no span yet" 0 (Tapper.count t')
        t'' <- Tapper.tap t'
        assertEqual "2nd tap" 1 (Tapper.count t'')
        t''' <- Tapper.tap t''
        assertEqual "3rd tap" 2 (Tapper.count t''')
        t'''' <- Tapper.tap t'''
        assertEqual "4th tap" 3 (Tapper.count t'''')
        t''''' <- Tapper.tap t''''
        assertEqual "5th tap, oldest dropped" 3 (Tapper.count t''''')

        t'''''' <- Tapper.tap (Tapper.toggleBounded t''''')
        assertEqual "1st tap after unbounding, can hold more" 4 (Tapper.count t'''''')
        t''''''' <- Tapper.tap t''''''
        assertEqual "2nd tap after unbounding" 5 (Tapper.count t''''''')
        let t'''''''' = Tapper.toggleBounded t'''''''
        assertBool "is now bounded" (Tapper.isBounded t'''''''')
        assertEqual "truncated after bounding" 3 (Tapper.count t'''''''')
    )

tapperTruncate :: Test
tapperTruncate =
  TestCase
    ( do
        let t = Tapper.create 3 True
        let t' = foldl Tapper.pushBpm t (map Sample [80.0, 70.0, 60.0])
        assertEqual "initialized" "[60.0, 70.0, 80.0]" (show t')

        let t'' = Tapper.pushBpm t' (Sample 50.0)
        assertEqual "push evicts oldest sample" "[50.0, 60.0, 70.0]" (show t'')

        let t''' = Tapper.pushBpm (Tapper.toggleBounded t'') (Sample 40.0)
        assertEqual "nothing evicted from unbounded buffer" "[40.0, 50.0, 60.0, 70.0]" (show t''')

        let t'''' = Tapper.toggleBounded t'''
        assertEqual "after bounding, oldest evicted" "[40.0, 50.0, 60.0]" (show t'''')
    )

resize :: Test
resize =
  TestCase
    ( do
        let t = Tapper.create 3 True
        let t' = foldl Tapper.pushBpm t (map Sample [80.0, 70.0, 60.0])
        assertEqual "initialized" "[60.0, 70.0, 80.0]" (show t')

        let t'' = Tapper.resize t' 2
        assertEqual "after resize, oldest evicted" "[60.0, 70.0]" (show t'')
        assertEqual "capacity is smaller now" 2 (Tapper.capacity t'')
    )

queueTests :: Test
queueTests =
  TestList
    [ TestLabel "push pop" pushPop,
      TestLabel "push clobber" pushClobber,
      TestLabel "clear" clear,
      TestLabel "truncate back" truncateBack
    ]

tapperTests :: Test
tapperTests =
  TestList
    [ TestLabel "display" display,
      TestLabel "is recording" isRecording,
      TestLabel "bpm" bpm,
      TestLabel "tap" tap,
      TestLabel "truncate" tapperTruncate,
      TestLabel "resize" resize
    ]

tests :: Test
tests = TestList [TestLabel "Queue" queueTests, TestLabel "Tapper" tapperTests]

main :: IO ()
main = runTestTTAndExit tests
