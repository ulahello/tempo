module Tapper
  ( Sample (Sample),
    Tapper,
    create,
    capacity,
    count,
    isRecording,
    isBounded,
    toggleBounded,
    clear,
    resize,
    pushBpm,
    tap,
    bpm,
  )
where

import Data.Ord
import Data.Time.Clock
import qualified Queue as Q
import Text.Printf

maxCapacity :: Int
maxCapacity = 0x1000

-- TODO: having to construct samples is annoying --- make it an alias?
-- but we want custom Show
newtype Sample = Sample Float

data Tapper = Tapper
  { samples :: Q.Queue Sample,
    prevTap :: Maybe UTCTime,
    capacity :: Int,
    isBounded :: Bool
  }

create :: Int -> Bool -> Tapper
create n b = syncCap (Tapper (Q.create maxCapacity) Nothing n b)

count :: Tapper -> Int
count t = length (samples t)

isRecording :: Tapper -> Bool
isRecording Tapper {prevTap = Nothing} = False
isRecording _ = True

syncCap :: Tapper -> Tapper
syncCap t =
  let capacity' = clamp (1, Q.capacity (samples t)) (capacity t)
      t' = t {capacity = capacity'}
   in if isBounded t'
        then
          t' {samples = Q.truncateBack (samples t') (capacity t')}
        else
          t'

toggleBounded :: Tapper -> Tapper
toggleBounded t = syncCap (t {isBounded = not (isBounded t)})

clear :: Tapper -> Tapper
clear t =
  t
    { samples = Q.clear (samples t),
      prevTap = Nothing
    }

resize :: Tapper -> Int -> Tapper
resize t n = syncCap (t {capacity = n})

pushBpm :: Tapper -> Sample -> Tapper
pushBpm t s = syncCap (t {samples = Q.push (samples t) s})

tap :: Tapper -> IO Tapper
tap t = do
  now <- getCurrentTime
  let t' = case prevTap t of
        Nothing -> t
        Just prev ->
          if prev < now
            then
              let elapsed = diffUTCTime now prev
                  elapsedSecs = realToFrac (nominalDiffTimeToSeconds elapsed)
                  s = Sample (60.0 / elapsedSecs)
               in pushBpm t s
            else
              t
  return t' {prevTap = Just now}

bpm :: Tapper -> Sample
bpm t =
  let (x, _) =
        foldl
          ( \(avg, idx) (Sample s) ->
              let avg' = avg + ((s - avg) / idx)
               in (avg', idx + 1)
          )
          (0.0, 1)
          (samples t)
   in Sample x

instance Show Sample where
  show (Sample s) = printf "%.1f" s

instance Show Tapper where
  show t = show (Q.qReverse (samples t))
