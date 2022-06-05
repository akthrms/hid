{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import qualified Colonnade
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable as Foldable
import Flow ((<.), (<|))
import qualified Fmt
import QuoteData
import StatReport
import qualified Text.Blaze.Colonnade as BlazeColonnade
import qualified Text.Blaze.Html.Renderer.Utf8 as Renderer
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attributes

viaFmt :: Fmt.Buildable a => a -> Html.Html
viaFmt =
  Html.text <. Fmt.pretty

columnStats :: Colonnade.Colonnade Colonnade.Headed StatEntry Html.Html
columnStats =
  mconcat
    [ Colonnade.headed "Quote Field" (Html.i <. Html.string <. show <. quoteField),
      Colonnade.headed "Mean" (viaFmt <. meanValue),
      Colonnade.headed "Min" (viaFmt <. minValue),
      Colonnade.headed "Max" (viaFmt <. maxValue),
      Colonnade.headed "Days between Min/Max" (viaFmt <. daysBetweenMinMax)
    ]

columnData :: Colonnade.Colonnade Colonnade.Headed QuoteData Html.Html
columnData =
  mconcat
    [ Colonnade.headed "Day" (viaFmt <. day),
      Colonnade.headed "Open" (viaFmt <. showPrice <. open),
      Colonnade.headed "Close" (viaFmt <. showPrice <. close),
      Colonnade.headed "High" (viaFmt <. showPrice <. close),
      Colonnade.headed "Low" (viaFmt <. showPrice <. low),
      Colonnade.headed "Volume" (viaFmt <. volume)
    ]

htmlReport ::
  (Functor t, Foldable t) =>
  String ->
  t QuoteData ->
  [StatEntry] ->
  [FilePath] ->
  ByteString.ByteString
htmlReport docTitle quotes statEntries images =
  Renderer.renderHtml
    <| do
      Html.head
        <| do
          Html.title <| Html.string docTitle
          Html.style tableStyle

      Html.body
        <| do
          Monad.unless (null images)
            <| do
              Html.h1 "Charts"
              Foldable.traverse_ ((Html.img !) <. Attributes.src <. Html.toValue) images

          Html.h1 "Statistic Report"
          BlazeColonnade.encodeHtmlTable mempty columnStats statEntries

          Html.h1 "Stock Quotes Data"
          BlazeColonnade.encodeHtmlTable mempty columnData quotes
  where
    tableStyle =
      "table {border-collapse: collapse}"
        <> "td, th {border: 1px solid black; padding: 5px}"
