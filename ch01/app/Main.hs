module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Flow ((<.), (<|), (|>))
import qualified System.Environment as Env

type Entry =
  (Text.Text, Int)

type Vocabulary =
  [Entry]

extractVocabulary :: Text.Text -> Vocabulary
extractVocabulary text =
  words
    |> List.sort
    |> List.group
    |> map buildEntry
  where
    words =
      text
        |> Text.words
        |> map cleanWord
        |> filter (not <. Text.null)
        |> map Text.toCaseFold
    buildEntry xs =
      case xs of
        (x : _) -> (x, length xs)
        [] -> error "unexpected"
    cleanWord = Text.dropAround (not <. Char.isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocabulary =
  do
    putStrLn "All words:"
    TextIO.putStrLn <| Text.unlines <| map fst vocabulary

processTextFile :: FilePath -> IO ()
processTextFile filename =
  do
    text <- TextIO.readFile filename
    let vocabulary = extractVocabulary text
    printAllWords vocabulary

main :: IO ()
main =
  do
    args <- Env.getArgs
    case args of
      [filename] ->
        processTextFile filename
      _ ->
        putStrLn "Usage: vocabulary filename"
