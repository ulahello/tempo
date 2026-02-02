module Queue
  ( Queue,
    create,
    capacity,
    isEmpty,
    isFull,
    tryPush,
    pop,
    push,
    clear,
    qReverse,
    truncateBack,
  )
where

import Data.Foldable
import Data.List
import Data.Maybe

data Queue a = Queue
  { front :: [a],
    back :: [a],
    capacity :: Int
  }

create :: Int -> Queue a
create = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty = null

isFull :: Queue a -> Bool
isFull q = length q == capacity q

rebalance :: Queue a -> Queue a
rebalance (Queue [] b n) = Queue (reverse b) [] n
rebalance q = q

tryPush :: Queue a -> a -> Maybe (Queue a)
tryPush q x =
  if isFull q
    then Nothing
    else Just q {back = x : back q}

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue [] [] _) = (Nothing, q)
pop q@(Queue [] _ _) = pop (rebalance q)
pop q@(Queue (x : xs) _ _) = (Just x, q {front = xs})

push :: Queue a -> a -> Queue a
push q x =
  let q' = if isFull q then snd (pop q) else q
   in fromJust (tryPush q' x)

clear :: Queue a -> Queue a
clear q = create (capacity q)

qReverse :: Queue a -> Queue a
qReverse (Queue f b n) = Queue b f n

truncateBack :: Queue a -> Int -> Queue a
truncateBack q newLen =
  if length q <= newLen
    then q
    else
      let (_, q') = pop q
       in truncateBack q' newLen

instance Foldable Queue where
  foldr f z q =
    let backAcc = foldr f z (reverse (back q))
     in foldr f backAcc (front q)

instance (Show a) => Show (Queue a) where
  show q = "[" ++ intercalate ", " (map show $ toList q) ++ "]"
