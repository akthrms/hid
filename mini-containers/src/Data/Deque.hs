{-# LANGUAGE NoImplicitPrelude #-}

module Data.Deque
  ( Deque,
    empty,
    isEmpty,
    front,
    back,
    pushFront,
    pushBack,
    popFront,
    popBack,
  )
where

import Data.Bool (Bool)
import Data.Maybe (Maybe (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

newtype Deque a
  = Deque (Seq a)

empty :: Deque a
empty =
  Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque seq) =
  Seq.null seq

front :: Deque a -> Maybe a
front deque =
  case deque of
    Deque (x :<| _) -> Just x
    Deque _ -> Nothing

back :: Deque a -> Maybe a
back deque =
  case deque of
    Deque (_ :|> x) -> Just x
    Deque _ -> Nothing

pushFront :: a -> Deque a -> Deque a
pushFront x (Deque xs) =
  Deque (x :<| xs)

pushBack :: a -> Deque a -> Deque a
pushBack x (Deque xs) =
  Deque (xs :|> x)

popFront :: Deque a -> Deque a
popFront deque =
  case deque of
    Deque (_ :<| xs) -> Deque xs
    Deque _ -> empty

popBack :: Deque a -> Deque a
popBack deque =
  case deque of
    Deque (xs :|> _) -> Deque xs
    Deque _ -> empty
