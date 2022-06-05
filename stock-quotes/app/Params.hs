module Params where

import Control.Applicative ((<**>))
import qualified Data.Text as Text
import Flow ((<|))
import qualified Options.Applicative as Ap

data Params = Params
  { filename :: FilePath,
    company :: Maybe Text.Text,
    chart :: Bool,
    htmlFile :: Maybe FilePath,
    silent :: Bool
  }

mkParams :: Ap.Parser Params
mkParams =
  Params
    <$> Ap.strArgument
      (Ap.metavar "FILE" <> Ap.help "CSV file name")
    <*> Ap.optional
      (Text.strip <$> Ap.strOption (Ap.long "name" <> Ap.short 'n' <> Ap.help "company name"))
    <*> Ap.switch
      (Ap.long "chart" <> Ap.short 'c' <> Ap.help "generate chart")
    <*> Ap.optional
      (Ap.strOption <| Ap.long "html" <> Ap.metavar "FILE" <> Ap.help "generate HTML report")
    <*> Ap.switch
      (Ap.long "silent" <> Ap.short 's' <> Ap.help "don't print statistics")

cmdLineParser :: IO Params
cmdLineParser =
  Ap.execParser opts
  where
    opts =
      Ap.info
        (mkParams <**> Ap.helper)
        (Ap.fullDesc <> Ap.progDesc "Stock quotes data processing")
