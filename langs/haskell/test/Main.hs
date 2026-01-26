module Main (main) where

import Data.Foldable
import qualified Queue
import qualified Tapper
import Tapper (Sample (Sample), Tapper)
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

-- TODO: use state variable instead of this. it doesn't handle the pushPop test well
runCaseList :: a -> [(a -> IO a, a -> IO ())] -> IO ()
runCaseList _ [] = return ()
runCaseList z ((modify, run) : rest) = do
  z' <- modify z
  run z'
  runCaseList z' rest

testOfCaseList :: a -> [(a -> IO a, a -> IO ())] -> Test
testOfCaseList z lst = TestCase (runCaseList z lst)

pushBpm' :: Float -> Tapper -> Tapper
pushBpm' = flip Tapper.pushBpm . Sample

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
  testOfCaseList
    (Queue.create 8)
    [ ( \q -> return (foldl Queue.push q (range (Queue.capacity q))),
        assertEqual "push works" [1, 2, 3, 4, 5, 6, 7, 8] . toList
      ),
      ( return . flip Queue.push 24,
        assertEqual "push clobbers oldest element when full" [2, 3, 4, 5, 6, 7, 8, 24] . toList
      )
    ]

clear :: Test
clear =
  testOfCaseList
    (Queue.create 8)
    [ ( \q -> return (foldl pushThreePopOne q (range (Queue.capacity q))),
        assertBool "not empty" . not . Queue.isEmpty
      ),
      (return . Queue.clear, assertBool "now is empty" . Queue.isEmpty),
      (return, assertEqual "empty contents" [] . toList)
    ]
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
  testOfCaseList
    (Queue.create 8)
    [ ( \q -> return (foldl Queue.push q (range (Queue.capacity q))),
        assertEqual "initialized" [1, 2, 3, 4, 5, 6, 7, 8] . toList
      ),
      (return . flip Queue.truncateBack 3, assertEqual "truncated down to 3" [6, 7, 8] . toList),
      (return . flip Queue.truncateBack 100, assertEqual "truncate up is nop" [6, 7, 8] . toList)
    ]

display :: Test
display =
  testOfCaseList
    (Tapper.create 10 True)
    [ ( \t -> return (foldl Tapper.pushBpm t (map Sample [120.051, 112.41, 121.105])),
        assertEqual "to string works" "[121.1, 112.4, 120.1]" . show
      ),
      (return . Tapper.clear, assertEqual "empty to string" "[]" . show),
      (return . pushBpm' 112.76, assertEqual "to string with one element works" "[112.8]" . show)
    ]

isRecording :: Test
isRecording =
  testOfCaseList
    (Tapper.create 10 True)
    [ (return, assertBool "starts not recording" . not . Tapper.isRecording),
      (Tapper.tap, assertBool "records first tap" . Tapper.isRecording),
      (Tapper.tap, assertBool "keeps recording taps" . Tapper.isRecording),
      (return . Tapper.clear, assertBool "stops once cleared" . not . Tapper.isRecording)
    ]

bpm :: Test
bpm =
  testOfCaseList
    (Tapper.create 10 True)
    [ (return, assertBool "starts at 0.0 BPM" . approxEqual 0.0 . Tapper.bpm),
      (return . pushBpm' 23.0, assertBool "1st tap" . approxEqual 23.0 . Tapper.bpm),
      (return . pushBpm' 26.0, assertBool "2nd tap" . approxEqual 24.5 . Tapper.bpm),
      (return . pushBpm' 29.0, assertBool "3rd tap" . approxEqual 26.0 . Tapper.bpm),
      (return . pushBpm' 61.0, assertBool "4th tap" . approxEqual 34.75 . Tapper.bpm)
    ]

tap :: Test
tap =
  testOfCaseList
    (Tapper.create 3 True)
    [ (return, assertEqual "starts empty" 0 . Tapper.count),
      (Tapper.tap, assertEqual "1st tap, no span yet" 0 . Tapper.count),
      (Tapper.tap, assertEqual "2nd tap" 1 . Tapper.count),
      (Tapper.tap, assertEqual "3rd tap" 2 . Tapper.count),
      (Tapper.tap, assertEqual "4th tap" 3 . Tapper.count),
      (Tapper.tap, assertEqual "5th tap, oldest dropped" 3 . Tapper.count),
      (Tapper.tap . Tapper.toggleBounded, assertEqual "1st tap after unbounding, can hold more" 4 . Tapper.count),
      (Tapper.tap, assertEqual "2nd tap after unbounding" 5 . Tapper.count),
      (return . Tapper.toggleBounded, assertBool "is now bounded" . Tapper.isBounded),
      (return, assertEqual "truncated after bounding" 3 . Tapper.count)
    ]

tapperTruncate :: Test
tapperTruncate =
  testOfCaseList
    (Tapper.create 3 True)
    [ ( \t -> return (foldl Tapper.pushBpm t (map Sample [80.0, 70.0, 60.0])),
        assertEqual "initialized" "[60.0, 70.0, 80.0]" . show
      ),
      ( return . pushBpm' 50.0,
        assertEqual "push evicts oldest sample" "[50.0, 60.0, 70.0]" . show
      ),
      ( return . pushBpm' 40.0,
        assertEqual "nothing evicted from unbounded buffer" "[40.0, 50.0, 60.0, 70.0]" . show
      ),
      ( return . Tapper.toggleBounded,
        assertEqual "after bounding, oldest evicted" "[40.0, 50.0, 60.0]" . show
      )
    ]

resize :: Test
resize =
  -- TODO: awful
  testOfCaseList
    (Tapper.create 3 True)
    [ ( \t -> return (foldl Tapper.pushBpm t (map Sample [80.0, 70.0, 60.0])),
        assertEqual "initialized" "[60.0, 70.0, 80.0]" . show
      ),
      ( return . flip Tapper.resize 2,
        assertEqual "after resize, oldest evicted" "[60.0, 70.0]" . show
      ),
      (return, assertEqual "capacity is smaller now" 2 . Tapper.capacity)
    ]

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
