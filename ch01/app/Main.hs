module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Flow ((|>))
import qualified System.Environment as Env

main :: IO ()
main =
  do
    [filename] <- Env.getArgs
    text <- TextIO.readFile filename
    let words =
          text
            |> Text.words
            |> map (Text.dropAround $ not . Char.isLetter)
            |> filter (not . Text.null)
            |> map Text.toCaseFold
            |> List.sort
            |> List.group
            |> map head
    TextIO.putStrLn $ Text.unwords words
    print $ length words
