{-# LANGUAGE DeriveAnyClass #-}

module Radar where

import Flow ((<.), (|>))

class (Bounded a, Eq a, Enum a) => CyclicEnum a where
  cyclicPred :: a -> a
  cyclicPred d
    | d == minBound = maxBound
    | otherwise = pred d

  cyclicSucc :: a -> a
  cyclicSucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction
  = North
  | East
  | South
  | West
  deriving (Bounded, CyclicEnum, Enum, Eq, Show)

data Turn
  = TurnNone
  | TurnLeft
  | TurnRight
  | TurnAround
  deriving (Bounded, Enum, Eq, Show)

rotate :: Turn -> Direction -> Direction
rotate turn =
  case turn of
    TurnNone ->
      id
    TurnLeft ->
      cyclicPred
    TurnRight ->
      cyclicSucc
    TurnAround ->
      cyclicPred <. cyclicPred

every :: (Bounded a, Enum a) => [a]
every =
  enumFrom minBound

orient :: Direction -> Direction -> Turn
orient direction1 direction2 =
  every
    |> filter (\t -> rotate t direction1 == direction2)
    |> head

rotateMany :: Direction -> [Turn] -> Direction
rotateMany =
  foldl (flip rotate)

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps =
  scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany directions =
  case directions of
    (_ : _ : _) ->
      zipWith orient directions (tail directions)
    _ ->
      []
