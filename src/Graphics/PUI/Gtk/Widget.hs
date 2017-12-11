{-# LANGUAGE
  FlexibleInstances
, MultiParamTypeClasses
, TypeFamilies
, TypeSynonymInstances
, ScopedTypeVariables
 #-}

module Graphics.PUI.Gtk.Widget
  ( Dim
  , CairoWidget
  , drawFlowWidget
  , StyleT, Style (..), RGB (..)
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

data RGB = RGB Double Double Double

data Style = Style
  { styleFont :: FontDescription
  , styleColor0 :: RGB
  , styleColor1 :: RGB
  }

type StyleT m = ReaderT Style m
type CairoWidget w h f = Widget w h f (Render ())

retain r = save >> r >> restore

leftof :: forall f h. (Monad f, VarDim h, ValueOf h ~ Dim)
       => CairoWidget (F Dim) h f -> CairoWidget (V Dim) (V Dim) f -> CairoWidget (V Dim) h f
leftof a b = Widget $ \w h -> do
  (w1, h1, r1) <- fromWidget a () h
  r2 <- fromFlow b (w - w1) (valueOf (Proxy :: Proxy h) h h1)
  return ((), h1, (retain r1 >> translate w1 0 >> retain r2))

topof :: forall f w. (Monad f, VarDim w, ValueOf w ~ Dim)
      => CairoWidget w (F Dim) f -> CairoWidget (V Dim) (V Dim) f -> CairoWidget w (V Dim) f
topof a b = Widget $ \w h -> do
  (w1, h1, r1) <- fromWidget a w ()
  r2 <- fromFlow b (valueOf (Proxy :: Proxy w) w w1) (h - h1)
  return (w1, (), (retain r1 >> translate 0 h1 >> retain r2))

alignleft :: (Monad f, VarDim h, ValueOf h ~ Dim) => CairoWidget (F Dim) h f -> CairoWidget (V Dim) h f
alignleft x = x `leftof` fill

aligntop :: (Monad f, VarDim w, ValueOf w ~ Dim) => CairoWidget w (F Dim) f -> CairoWidget w (V Dim) f
aligntop x = x `topof` fill

box :: forall f w h. (Monad f, VarDim w, VarDim h, ValueOf w ~ Dim, ValueOf h ~ Dim)
    => CairoWidget w h (StyleT f) -> CairoWidget w h (StyleT f)
box x = Widget $ \w h -> do
  (rw, rh, r) <- fromWidget x w h
  col <- asks styleColor1
  let bw = valueOf (Proxy :: Proxy w) w rw
      bh = valueOf (Proxy :: Proxy h) h rh
      drawit = do
        setSourceRGB' col
        rectangle 0 0 bw bh
        stroke
        retain r
  return (rw, rh, drawit)

drawFlowWidget :: (Functor f) => CairoWidget (V w) (V h) (StyleT f) -> w -> h -> Style -> f (Render ())
drawFlowWidget widget w h style = fromFlow (hoistWidget (\x -> runReaderT x style) widget) w h

setSourceRGB' (RGB a b c) = setSourceRGB a b c

text :: (MonadIO f) => String -> CairoWidget (F Dim) (F Dim) (StyleT f)
text str = mkFixed $ do
  fontdesc <- asks styleFont
  textcolor <- asks styleColor1
  context <- liftIO $ cairoCreateContext Nothing
  layout <- liftIO $ layoutText context str
  liftIO $ layoutSetFontDescription layout (Just fontdesc)
  (_, PangoRectangle x y w h) <- liftIO $ layoutGetExtents layout
  let drawit = do
        setSourceRGB' textcolor
        showLayout layout
  return (w, h, drawit)

fill :: (Applicative f) => CairoWidget (V w) (V h) f
fill = mkFlow $ \w h -> pure (return ())
