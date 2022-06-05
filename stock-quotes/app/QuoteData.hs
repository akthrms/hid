{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module QuoteData where

import qualified Data.ByteString.Char8 as BSChar
import qualified Data.Csv as Csv
import qualified Data.Time as Time
import Flow ((<.))
import qualified GHC.Generics as Generics

data QuoteData = QuoteData
  { day :: Time.Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
  }
  deriving (Generics.Generic, Csv.FromNamedRecord)

instance Csv.FromField Time.Day where
  parseField =
    Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" <. BSChar.unpack

data QuoteField
  = Open
  | Close
  | High
  | Low
  | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

fieldToFunction :: QuoteField -> QuoteData -> Double
fieldToFunction Open = open
fieldToFunction Close = close
fieldToFunction High = high
fieldToFunction Low = low
fieldToFunction Volume = fromIntegral <. volume
