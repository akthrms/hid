{-# LANGUAGE RecordWildCards #-}

module StatReport where

import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
import qualified Data.Time as Time
import Flow ((<|), (|>))
import QuoteData

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue
  { decimalPlaces :: Int,
    value :: Double
  }

data StatEntry = StatEntry
  { quoteField :: QuoteField,
    meanValue :: StatValue,
    minValue :: StatValue,
    maxValue :: StatValue,
    daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs =
  sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t) => (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get quotes =
  (get minQuote, get maxQuote, days)
  where
    cmp =
      Ord.comparing get

    minQuote =
      Foldable.minimumBy cmp quotes

    maxQuote =
      Foldable.maximumBy cmp quotes

    days =
      Time.diffDays (day minQuote) (day maxQuote)
        |> abs
        |> fromIntegral

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes =
  fmap quoteFieldStatInfo [minBound .. maxBound]
  where
    decimalPlacesByQuoteField Volume = 0
    decimalPlacesByQuoteField _ = decimalPlacesFloating

    quoteFieldStatInfo quoteField =
      let get = fieldToFunction quoteField
          (mn, mx, daysBetweenMinMax) = computeMinMaxDays get quotes
          decimalPlaces = decimalPlacesByQuoteField quoteField
          meanValue = StatValue decimalPlacesFloating (mean <| fmap get quotes)
          minValue = StatValue decimalPlaces mn
          maxValue = StatValue decimalPlaces mx
       in StatEntry {..}
