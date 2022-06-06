module Data.Stack
  ( Stack,
    empty,
    isEmpty,
    push,
    pop,
    top,
  )
where

newtype Stack a
  = Stack [a]

empty :: Stack a
empty =
  Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) =
  null xs

push :: a -> Stack a -> Stack a
push x (Stack xs) =
  Stack (x : xs)

pop :: Stack a -> Stack a
pop stack =
  case stack of
    Stack [] -> Stack []
    Stack (_ : xs) -> Stack xs

top :: Stack a -> Maybe a
top stack =
  case stack of
    Stack [] -> Nothing
    Stack (x : _) -> Just x
