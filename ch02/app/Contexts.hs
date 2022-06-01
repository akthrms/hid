module Contexts where

import qualified Control.Monad.Writer as Writer
import Flow ((<|))

readNumber :: IO Int
readNumber =
  do
    s <- getLine
    pure <| read s

sumN :: Int -> Writer.Writer String Int
sumN 0 = Writer.writer (0, "finish")
sumN n =
  do
    Writer.tell <| show n ++ ","
    s <- sumN (n - 1)
    pure <| n + s

cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs ys =
  do
    x <- xs
    y <- ys
    pure <| (x, y)

addNumber :: Int -> IO String
addNumber n =
  pure (++) <*> pure (show n ++ " ") <*> getLine
