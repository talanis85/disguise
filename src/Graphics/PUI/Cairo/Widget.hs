{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Graphics.PUI.Cairo.Widget
Description : Widgets to be displayed with Cairo
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Graphics.PUI.Cairo.Widget
  ( 
  -- * Definition of Cairo widgets
    Dim
  , CairoWidget
  , StyleT, Style (..), RGB (..)

  -- * Consumption
  , drawFlowWidget

  -- * Basic layout combinators
  , leftOf, topOf
  , alignLeft, alignTop
  , space
  , stretchH, stretchV
  , box

  -- * Helpers
  , setSourceRGB'
  ) where

import Control.Monad.Reader
import Data.Functor.Identity
import Graphics.PUI.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

-- | Cairo uses 'Double' for coordinates
type Dim = Double

-- | Color type
data RGB = RGB Double Double Double

-- | Style type
data Style = Style
  { styleFont :: FontDescription
  , styleColor0 :: RGB
  , styleColor1 :: RGB
  , styleColor2 :: RGB
  }

type StyleT m = ReaderT Style m
type CairoWidget w h f = Widget w h f (Render ())

retain r = save >> r >> restore

-- | Convert a 'FlowWidget' to a Cairo render action
drawFlowWidget :: (Functor f) => CairoWidget (V w) (V h) (StyleT f) -> w -> h -> Style -> f (Render ())
drawFlowWidget widget w h style = fromFlow (hoistWidget (\x -> runReaderT x style) widget) w h

-- | Arrange one widget on the left of another
leftOf :: forall f h. (Monad f, VarDim h, ValueOf h ~ Dim)
       => CairoWidget (F Dim) h f -> CairoWidget (V Dim) (V Dim) f -> CairoWidget (V Dim) h f
leftOf a b = Widget $ \w h -> do
  (w1, h1, r1) <- fromWidget a () h
  r2 <- fromFlow b (w - w1) (valueOf (Proxy :: Proxy h) h h1)
  return ((), h1, (retain r1 >> translate w1 0 >> retain r2))

-- | Arrange one widget on top of another
topOf :: forall f w. (Monad f, VarDim w, ValueOf w ~ Dim)
      => CairoWidget w (F Dim) f -> CairoWidget (V Dim) (V Dim) f -> CairoWidget w (V Dim) f
topOf a b = Widget $ \w h -> do
  (w1, h1, r1) <- fromWidget a w ()
  r2 <- fromFlow b (valueOf (Proxy :: Proxy w) w w1) (h - h1)
  return (w1, (), (retain r1 >> translate 0 h1 >> retain r2))

-- | Expand a widget horizontally by adding a space to the right
alignLeft :: (Monad f, VarDim h, ValueOf h ~ Dim) => CairoWidget (F Dim) h f -> CairoWidget (V Dim) h f
alignLeft x = x `leftOf` space

-- | Expand a widget vertically by adding a space to the bottom
alignTop :: (Monad f, VarDim w, ValueOf w ~ Dim) => CairoWidget w (F Dim) f -> CairoWidget w (V Dim) f
alignTop x = x `topOf` space

-- | Expand a widget vertically by scaling along the Y axis
stretchV :: forall f w. (Monad f) => CairoWidget w (F Dim) f -> CairoWidget w (V Dim) f
stretchV widget = Widget $ \w h -> do
  (w', h', r) <- fromWidget widget w ()
  let drawit = do
        scale 1.0 (h / h')
        retain r
  return (w', (), drawit)

-- | Expand a widget horizontally by scaling along the X axis
stretchH :: forall f h. (Monad f) => CairoWidget (F Dim) h f -> CairoWidget (V Dim) h f
stretchH widget = Widget $ \w h -> do
  (w', h', r) <- fromWidget widget () h
  let drawit = do
        scale (w / w') 1.0
        retain r
  return ((), h', drawit)

-- | Draw a box around a widget
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

-- | A space of variable size
space :: (Applicative f) => CairoWidget (V w) (V h) f
space = mkFlow $ \w h -> pure (return ())

setSourceRGB' (RGB a b c) = setSourceRGB a b c

{-
withKeyString :: String -> CairoWidget w h f -> CairoWidget w h f
withKeyString str widget = Widget $ \w h -> do
  (rw, rh, r) <- fromWidget widget w h
  fontdesc <- asks styleFont
  textcolor <- asks styleColor1
  context <- liftIO $ cairoCreateContext Nothing
  layout <- liftIO $ layoutText context str
  let bw = valueOf (Proxy :: Proxy w) w rw
      bh = valueOf (Proxy :: Proxy h) h rh
      drawit = do
        retain r
        setSourceRGB' textcolor
        showLayout layout
  return (rw, rh, drawit)
-}
