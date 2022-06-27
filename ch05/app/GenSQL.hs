{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Flow

type SQL = Text

data ErrorMessage = WrongFormat Int Text deriving (Show)

genInsert :: Text -> Text -> Text
genInsert s1 s2 =
  "INSERT INTO items VALUES ('" <> s1 <> "', '" <> s2 <> "');\n"

processLine :: (Int, Text) -> Writer [ErrorMessage] SQL
processLine (_, splitOn ":" -> [s1, s2]) = genInsert s1 s2 |> pure
processLine (i, s) = tell [WrongFormat i s] >> pure ""

genSQL :: Text -> Writer [ErrorMessage] SQL
genSQL txt =
  T.concat <$> traverse processLine (zip [1 ..] <| T.lines txt)

testData :: Text
testData =
  "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL =
  do
    let (sql, errors) = runWriter <| genSQL testData

    TIO.putStrLn "SQL:"
    TIO.putStr sql

    TIO.putStrLn "Errors:"
    traverse_ print errors

main :: IO ()
main =
  testGenSQL
