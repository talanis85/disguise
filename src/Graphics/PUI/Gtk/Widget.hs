{-# LANGUAGE
  FlexibleInstances
, MultiParamTypeClasses
, TypeFamilies
, TypeSynonymInstances
, ScopedTypeVariables
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
  , alignleft, aligntop
  , text
  , fill
  , box
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

leftof :: forall h. (VarDim h, ValueOf h ~ Dim)
       => Widget (F Dim) h PUI () -> Widget (V Dim) (V Dim) PUI () -> Widget (V Dim) h PUI ()
leftof a b = Widget $ \w h -> do
  lift save
  (w1, h1, ()) <- fromWidget a () h
  lift $ translate w1 0
  () <- fromFlow b (w - w1) (valueOf (Proxy :: Proxy h) h h1)
  lift restore
  return ((), h1, ())

topof :: forall w. (VarDim w, ValueOf w ~ Dim)
      => Widget w (F Dim) PUI () -> Widget (V Dim) (V Dim) PUI () -> Widget w (V Dim) PUI ()
topof a b = Widget $ \w h -> do
  lift save
  (w1, h1, ()) <- fromWidget a w ()
  lift $ translate 0 h1
  () <- fromFlow b (valueOf (Proxy :: Proxy w) w w1) (h - h1)
  lift restore
  return (w1, (), ())

alignleft :: (VarDim h, ValueOf h ~ Dim) => Widget (F Dim) h PUI () -> Widget (V Dim) h PUI ()
alignleft x = x `leftof` fill

aligntop :: (VarDim w, ValueOf w ~ Dim) => Widget w (F Dim) PUI () -> Widget w (V Dim) PUI ()
aligntop x = x `topof` fill

box :: forall w h. (VarDim w, VarDim h, ValueOf w ~ Dim, ValueOf h ~ Dim)
    => Widget w h PUI () -> Widget w h PUI ()
box x = Widget $ \w h -> do
  col <- asks puiColor1
  (rw, rh, ()) <- fromWidget x w h
  let bw = valueOf (Proxy :: Proxy w) w rw
      bh = valueOf (Proxy :: Proxy h) h rh
  lift $ do
    setSourceRGB' col
    rectangle 0 0 bw bh
    stroke
  return (rw, rh, ())

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
