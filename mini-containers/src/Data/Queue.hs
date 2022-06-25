{-# LANGUAGE NoImplicitPrelude #-}

module Data.Queue
  ( Queue,
    empty,
    isEmpty,
    front,
    enqueue,
    dequeue,
  )
where

import Data.Bool (Bool)
import Data.Deque (Deque)
import qualified Data.Deque as Deque
import Data.Maybe (Maybe (..))
import Flow ((|>))

newtype Queue a
  = Queue (Deque a)

empty :: Queue a
empty =
  Queue Deque.empty

isEmpty :: Queue a -> Bool
isEmpty (Queue deque) =
  Deque.isEmpty deque

front :: Queue a -> Maybe a
front (Queue deque) =
  Deque.front deque

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue deque) =
  deque
    |> Deque.pushBack x
    |> Queue

dequeue :: Queue a -> Queue a
dequeue (Queue deque) =
  deque
    |> Deque.popFront
    |> Queue
