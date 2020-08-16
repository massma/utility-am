{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module GnuplotHelpers
  ( plotTermFrame,
    plotPairs,
    plotPairsLabels,
    plotPairsPoints,
    plotErrorBars,
    plot3d,
  )
where

-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
-- import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import Graphics.Gnuplot.Advanced
  ( fileContents,
    plot,
    plotAsync,
  )
import qualified Graphics.Gnuplot.Display as Display
import qualified Graphics.Gnuplot.Frame as Frame
import Graphics.Gnuplot.Frame.Option
  ( custom,
    pm3d,
  )
import qualified Graphics.Gnuplot.Frame.OptionSet as OSet
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as G3d
import qualified Graphics.Gnuplot.Graph.TwoDimensional as G2d
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as P3d
import qualified Graphics.Gnuplot.Plot.TwoDimensional as P2d
import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import Prelude

globalLineSpec = LineSpec.lineWidth 2.0

plotTermFrame ::
  (Terminal.C terminal, Display.C gfx) => terminal -> gfx -> IO ()
plotTermFrame term' frame =
  plot term' frame >>= putStrLn . show >> putStrLn str
  where
    -- plotAsync term' frame >>= putStrLn . show >> putStrLn str
    (str, _ts) = fileContents "./" term' frame

plotPairs fpath xlabel ylabel title xss =
  plotTermFrame
    (PNG.cons fpath)
    ( Frame.cons
        (OSet.title title . OSet.xLabel xlabel . OSet.yLabel ylabel $ OSet.deflt)
        ( fmap (G2d.lineSpec (globalLineSpec . LineSpec.title "" $ LineSpec.deflt)) $
            foldMap (P2d.list G2d.lines) xss
        )
    )

plotPairsLabels fpath xlabel ylabel title labxss =
  plotTermFrame
    (PNG.cons fpath)
    ( Frame.cons
        (OSet.title title . OSet.xLabel xlabel . OSet.yLabel ylabel $ OSet.deflt)
        ( foldMap
            ( \(lab, xs) ->
                fmap
                  (G2d.lineSpec (globalLineSpec . LineSpec.title lab $ LineSpec.deflt))
                  (P2d.list G2d.lines xs)
            )
            labxss
        )
    )

plotPairsPoints fpath xlabel ylabel title xss =
  plotTermFrame
    (PNG.cons fpath)
    ( Frame.cons
        (OSet.title title . OSet.xLabel xlabel . OSet.yLabel ylabel $ OSet.deflt)
        ( fmap (G2d.lineSpec (globalLineSpec . LineSpec.title "" $ LineSpec.deflt)) $
            foldMap (P2d.list G2d.linesPoints) xss
        )
    )

plotErrorBars fpath xlabel ylabel title xs =
  plotTermFrame
    (PNG.cons fpath)
    ( Frame.cons
        (OSet.title title . OSet.xLabel xlabel . OSet.yLabel ylabel $ OSet.deflt)
        ( fmap (G2d.lineSpec (globalLineSpec . LineSpec.title "" $ LineSpec.deflt)) $
            (P2d.list G2d.xyErrorBarsRelative xs)
        )
    )

plot3d fpath xlabel ylabel title xs ys xyToZ =
  plotTermFrame
    (PNG.cons fpath)
    ( Frame.cons
        ( OSet.key False
            . OSet.title title
            . OSet.viewMap
            . OSet.xLabel xlabel
            . OSet.yLabel ylabel
            . OSet.add (pm3d "") ["corners2color", "c1"]
            $ OSet.deflt
        )
        (P3d.surface xs ys xyToZ)
    )
