{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Flow ((<.), (<|), (|>))
import Fmt ((+|), (|+))
import qualified Fmt
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

allWords :: Vocabulary -> [Text.Text]
allWords =
  map fst

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocabulary =
  (sum <| map snd vocabulary, length vocabulary)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency =
  List.sortBy (Ord.comparing <| Ord.Down <. snd)

allWordsReport :: Vocabulary -> Text.Text
allWordsReport vocabulary =
  Fmt.fmt <| Fmt.nameF "All words" <| Fmt.unlinesF <| allWords vocabulary

wordsCountReport :: Vocabulary -> Text.Text
wordsCountReport vocabulary =
  Fmt.fmt <| "Total number of words: " +| total |+ "\nNumber of unique words: " +| unique |+ "\n"
  where
    (total, unique) = wordsCount vocabulary

frequentWordsReport :: Vocabulary -> Int -> Text.Text
frequentWordsReport vocabulary n =
  Fmt.fmt <| Fmt.nameF "Frequent words" <| Fmt.blockListF' "" fmtEntry reportData
  where
    reportData = take n <| wordsByFrequency vocabulary
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

printAllWords :: Vocabulary -> IO ()
printAllWords vocabulary =
  do
    putStrLn "All words:"
    TextIO.putStrLn <| Text.unlines <| map fst vocabulary

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile filename withAllWords n =
  do
    text <- TextIO.readFile filename
    let vocabulary = extractVocabulary text
    when withAllWords <| TextIO.putStrLn <| allWordsReport vocabulary
    TextIO.putStrLn <| wordsCountReport vocabulary
    TextIO.putStrLn <| frequentWordsReport vocabulary n

main :: IO ()
main =
  do
    args <- Env.getArgs
    case args of
      ["-a", filename, n] ->
        processTextFile filename True (read n)
      [filename, n] ->
        processTextFile filename False (read n)
      _ ->
        putStrLn "Usage: vocabulary [-a] <filename> <freq-words-num>"
