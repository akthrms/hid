{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Charts
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv as Csv
import qualified Data.Text as Text
import Flow ((<|))
import HtmlReport
import Params
import QuoteData
import StatReport

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params =
  do
    csvData <- BS.readFile (filename params)
    case Csv.decodeByName csvData of
      Left err -> putStrLn err
      Right (_, quotes) -> generateReports params quotes

generateReports :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReports Params {..} quotes =
  do
    Monad.unless silent <| putStrLn textReport'
    Monad.when chart <| plotChart title quotes chartFilename
    saveHtml htmlFile htmlReport'
  where
    statInfo' =
      statInfo quotes

    textReport' =
      textReport statInfo'

    htmlReport' =
      htmlReport title quotes statInfo' [chartFilename | chart]

    withCompany prefix =
      maybe mempty (prefix <>) company

    chartFilename =
      Text.unpack <| "chart" <> withCompany "_" <> ".svg"

    title =
      Text.unpack <| "Historical Quotes" <> withCompany " for "

    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BS.writeFile f html
