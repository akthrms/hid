module Params where

import Control.Applicative ((<**>))
import qualified Data.Text as T
import Flow ((<|))
import qualified Options.Applicative as OP

data Params = Params
  { filename :: FilePath,
    company :: Maybe T.Text,
    chart :: Bool,
    htmlFile :: Maybe FilePath,
    silent :: Bool
  }

mkParams :: OP.Parser Params
mkParams =
  Params
    <$> OP.strArgument
      (OP.metavar "FILE" <> OP.help "CSV file name")
    <*> OP.optional
      (T.strip <$> OP.strOption (OP.long "name" <> OP.short 'n' <> OP.help "company name"))
    <*> OP.switch
      (OP.long "chart" <> OP.short 'c' <> OP.help "generate chart")
    <*> OP.optional
      (OP.strOption <| OP.long "html" <> OP.metavar "FILE" <> OP.help "generate HTML report")
    <*> OP.switch
      (OP.long "silent" <> OP.short 's' <> OP.help "don't print statistics")

cmdLineParser :: IO Params
cmdLineParser =
  OP.execParser opts
  where
    opts =
      OP.info
        (mkParams <**> OP.helper)
        (OP.fullDesc <> OP.progDesc "Stock quotes data processing")
