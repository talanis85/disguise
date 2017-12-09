{-# LANGUAGE
  FlexibleInstances
, MultiParamTypeClasses
, TypeSynonymInstances
 #-}

module Graphics.PUI.Gtk.Widget
  ( Dim
  , GtkFlowWidget
  , GtkFixedWidget
  , GtkFixedHeightWidget
  , GtkFixedWidthWidget
  , drawFlowWidget
  , PUI, PUIOptions (..), RGB (..)
  , setSourceRGB'

  , leftof, topof
  , text
  , fill
  ) where

import Control.Monad.Reader
import Data.Functor.Identity
import Graphics.PUI.Widget
import Graphics.Rendering.Cairo hiding (fill)
import Graphics.Rendering.Pango

type Dim = Double
type GtkFlowWidget = FlowWidget Dim PUI ()
type GtkFixedWidget = FixedWidget Dim PUI ()
type GtkFixedWidthWidget = FixedWidthWidget Dim PUI ()
type GtkFixedHeightWidget = FixedHeightWidget Dim PUI ()

data RGB = RGB Double Double Double

data PUIOptions = PUIOptions
  { puiFont :: FontDescription
  , puiColor0 :: RGB
  , puiColor1 :: RGB
  }

type PUI = ReaderT PUIOptions Render

leftof :: GtkFixedWidget -> GtkFlowWidget -> GtkFixedHeightWidget
leftof a b = mkFixedHeight $ \w -> do
  lift save
  (w1, h1, ()) <- fromFixed a
  lift $ translate w1 0
  () <- fromFlow b (w - w1) h1
  lift restore
  return (h1, ())

topof :: GtkFixedHeightWidget -> GtkFlowWidget -> GtkFlowWidget
topof a b = mkFlow $ \w h -> do
  lift save
  (h1, ()) <- fromFixedHeight a w
  lift $ translate 0 h1
  () <- fromFlow b w (h - h1)
  lift restore
  return ()

drawFlowWidget :: GtkFlowWidget -> Dim -> Dim -> PUIOptions -> Render ()
drawFlowWidget widget w h opts = runReaderT (fromFlow widget w h) opts

setSourceRGB' (RGB a b c) = setSourceRGB a b c

text :: String -> GtkFixedWidget
text str = mkFixed $ do
  fontdesc <- asks puiFont
  textcolor <- asks puiColor1
  lift $ do
    layout <- createLayout str
    liftIO $ layoutSetFontDescription layout (Just fontdesc)
    setSourceRGB' textcolor
    showLayout layout
    (_, PangoRectangle x y w h) <- liftIO $ layoutGetExtents layout
    return (w, h, ())

fill :: GtkFlowWidget
fill = mkFlow $ \w h -> do
  return ()
