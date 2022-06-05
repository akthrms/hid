{-# LANGUAGE RecordWildCards #-}

module Charts where

import qualified Data.Foldable as Foldable
import Flow ((<|), (|>))
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import Graphics.Rendering.Chart.Easy ((.~))
import qualified Graphics.Rendering.Chart.Easy as Chart
import QuoteData

plotChart :: Foldable.Foldable f => String -> f QuoteData -> FilePath -> IO ()
plotChart title quotes filename =
  do
    _ <- Diagrams.renderableToFile fileOptions filename (Chart.toRenderable chart)
    pure ()
  where
    fileOptions =
      Diagrams.FileOptions (800, 600) Diagrams.SVG Diagrams.loadSansSerifFonts

    (candles, closings, volumes) =
      unzip3
        <| [ (Chart.Candle day low open 0 close high, (day, close), (day, [volume]))
             | QuoteData {..} <- Foldable.toList quotes
           ]

    chart =
      Chart.def
        |> Chart.slayouts_layouts
          .~ [ Chart.StackedLayout candlesLayout,
               Chart.StackedLayout volumesLayout
             ]

    candlesLayout =
      Chart.def
        |> Chart.layout_title .~ title
        |> Chart.layout_plots
          .~ [ Chart.toPlot <| quoteLine "Close" closings Chart.green,
               Chart.toPlot <| candle "Candle" candles Chart.cyan
             ]

    volumesLayout =
      Chart.def
        |> Chart.layout_plots
          .~ [Chart.plotBars <| bars "Volume" volumes Chart.gray]

    candle title values color =
      Chart.def
        |> Chart.plot_candle_line_style .~ lineStyle 1 Chart.gray
        |> Chart.plot_candle_fill .~ True
        |> Chart.plot_candle_rise_fill_style .~ fillStyle Chart.white
        |> Chart.plot_candle_fall_fill_style .~ fillStyle color
        |> Chart.plot_candle_tick_length .~ 0
        |> Chart.plot_candle_width .~ 3
        |> Chart.plot_candle_values .~ values
        |> Chart.plot_candle_title .~ title

    quoteLine title values color =
      Chart.def
        |> Chart.plot_lines_style .~ lineStyle 1 color
        |> Chart.plot_lines_values .~ [values]
        |> Chart.plot_lines_title .~ title

    bars title values color =
      Chart.def
        |> Chart.plot_bars_titles .~ [title]
        |> Chart.plot_bars_values .~ values
        |> Chart.plot_bars_item_styles .~ [(fillStyle color, Nothing)]

    fillStyle color =
      Chart.solidFillStyle (Chart.opaque color)

    lineStyle n color =
      Chart.def
        |> Chart.line_width .~ n
        |> Chart.line_color .~ Chart.opaque color
