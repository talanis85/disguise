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
  , runStyleT

  -- * Basic layout combinators
  , leftOf, topOf, rightOf, bottomOf
  , alignLeft, alignTop
  , space, spaceH, spaceV
  , stretchH, stretchV
  , box

  -- * Helpers
  , setSourceRGB'
  , retain
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

runStyleT :: StyleT m a -> Style -> m a
runStyleT = runReaderT

type CairoWidget w h f = Widget w h f (Render ())

retain r = save >> r >> restore

-- | Convert a 'FlowWidget' to a Cairo render action
drawFlowWidget :: (Functor f) => CairoWidget (V w) (V h) (StyleT f) -> w -> h -> Style -> f (Render ())
drawFlowWidget widget w h style = case hoistWidget (\x -> runReaderT x style) widget of
  FlowWidget widget' -> widget' w h

-- | Arrange one widget on the left of another
leftOf :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim)
       => CairoWidget (F Dim) h f -> CairoWidget w (V Dim) f -> CairoWidget w h f
leftOf (FixedWidget a) (FlowWidget b) = FixedHeightWidget $ \w -> do
  (w1, h1, r1) <- a
  r2 <- b (w - w1) h1
  return (h1, (retain r1 >> translate w1 0 >> retain r2))
leftOf (FixedWidthWidget a) (FlowWidget b) = FlowWidget $ \w h -> do
  (w1, r1) <- a h
  r2 <- b (w - w1) h
  return (retain r1 >> translate w1 0 >> retain r2)
leftOf (FixedWidget a) (FixedWidthWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (w2, r2) <- b h1
  return (w1 + w2, h1, retain r1 >> translate w1 0 >> retain r2)
leftOf (FixedWidthWidget a) (FixedWidthWidget b) = FixedWidthWidget $ \h -> do
  (w1, r1) <- a h
  (w2, r2) <- b h
  return (w1 + w2, retain r1 >> translate w1 0 >> retain r2)

-- | Arrange one widget on top of another
topOf :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim)
      => CairoWidget w (F Dim) f -> CairoWidget (V Dim) h f -> CairoWidget w h f
topOf (FixedWidget a) (FlowWidget b) = FixedWidthWidget $ \h -> do
  (w1, h1, r1) <- a
  r2 <- b w1 (h - h1)
  return (w1, (retain r1 >> translate 0 h1 >> retain r2))
topOf (FixedHeightWidget a) (FlowWidget b) = FlowWidget $ \w h -> do
  (h1, r1) <- a w
  r2 <- b w (h - h1)
  return (retain r1 >> translate 0 h1 >> retain r2)
topOf (FixedWidget a) (FixedHeightWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (h2, r2) <- b w1
  return (w1, h1 + h2, retain r1 >> translate 0 h1 >> retain r2)
topOf (FixedHeightWidget a) (FixedHeightWidget b) = FixedHeightWidget $ \w -> do
  (h1, r1) <- a w
  (h2, r2) <- b w
  return (h1 + h2, retain r1 >> translate 0 h1 >> retain r2)

-- | Arrange one widget on the right of another
rightOf :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim)
       => CairoWidget (F Dim) h f -> CairoWidget w (V Dim) f -> CairoWidget w h f
rightOf (FixedWidget a) (FlowWidget b) = FixedHeightWidget $ \w -> do
  (w1, h1, r1) <- a
  r2 <- b (w - w1) h1
  return (h1, (retain r2 >> translate (w - w1) 0 >> retain r1))
rightOf (FixedWidthWidget a) (FlowWidget b) = FlowWidget $ \w h -> do
  (w1, r1) <- a h
  r2 <- b (w - w1) h
  return (retain r2 >> translate (w - w1) 0 >> retain r1)
rightOf (FixedWidget a) (FixedWidthWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (w2, r2) <- b h1
  return (w1 + w2, h1, retain r2 >> translate w2 0 >> retain r1)
rightOf (FixedWidthWidget a) (FixedWidthWidget b) = FixedWidthWidget $ \h -> do
  (w1, r1) <- a h
  (w2, r2) <- b h
  return (w1 + w2, retain r2 >> translate w2 0 >> retain r1)

-- | Arrange one widget at the bottom of another
bottomOf :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim)
      => CairoWidget w (F Dim) f -> CairoWidget (V Dim) h f -> CairoWidget w h f
bottomOf (FixedWidget a) (FlowWidget b) = FixedWidthWidget $ \h -> do
  (w1, h1, r1) <- a
  r2 <- b w1 (h - h1)
  return (w1, (retain r2 >> translate 0 (h - h1) >> retain r1))
bottomOf (FixedHeightWidget a) (FlowWidget b) = FlowWidget $ \w h -> do
  (h1, r1) <- a w
  r2 <- b w (h - h1)
  return (retain r2 >> translate 0 (h - h1) >> retain r1)
bottomOf (FixedWidget a) (FixedHeightWidget b) = FixedWidget $ do
  (w1, h1, r1) <- a
  (h2, r2) <- b w1
  return (w1, h1 + h2, retain r2 >> translate 0 h2 >> retain r1)
bottomOf (FixedHeightWidget a) (FixedHeightWidget b) = FixedHeightWidget $ \w -> do
  (h1, r1) <- a w
  (h2, r2) <- b w
  return (h1 + h2, retain r2 >> translate 0 h2 >> retain r1)

-- | Expand a widget horizontally by adding a space to the right
alignLeft :: (Monad f, DimOf h ~ Dim) => CairoWidget (F Dim) h f -> CairoWidget (V Dim) h f
alignLeft x = x `leftOf` space

-- | Expand a widget vertically by adding a space to the bottom
alignTop :: (Monad f, DimOf w ~ Dim) => CairoWidget w (F Dim) f -> CairoWidget w (V Dim) f
alignTop x = x `topOf` space

-- | Expand a widget vertically by scaling along the Y axis
stretchV :: (Monad f) => CairoWidget w (F Dim) f -> CairoWidget w (V Dim) f
stretchV (FixedWidget widget) = FixedWidthWidget $ \h -> do
  (w', h', r) <- widget
  let drawit = do
        scale 1.0 (h / h')
        retain r
  return (w', drawit)
stretchV (FixedHeightWidget widget) = FlowWidget $ \w h -> do
  (h', r) <- widget w
  let drawit = do
        scale 1.0 (h / h')
        retain r
  return drawit

-- | Expand a widget horizontally by scaling along the X axis
stretchH :: (Monad f) => CairoWidget (F Dim) h f -> CairoWidget (V Dim) h f
stretchH (FixedWidget widget) = FixedHeightWidget $ \w -> do
  (w', h', r) <- widget
  let drawit = do
        scale (w / w') 1.0
        retain r
  return (h', drawit)
stretchH (FixedWidthWidget widget) = FlowWidget $ \w h -> do
  (w', r) <- widget h
  let drawit = do
        scale (w / w') 1.0
        retain r
  return drawit

-- | Draw a box around a widget
box :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim) => CairoWidget w h (StyleT f) -> CairoWidget w h (StyleT f)
box (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget w h
  col <- asks styleColor1
  return (drawBox col w h r)
box (FixedWidget widget) = FixedWidget $ do
  (w, h, r) <- widget
  col <- asks styleColor1
  return (w, h, drawBox col w h r)
box (FixedWidthWidget widget) = FixedWidthWidget $ \h -> do
  (w, r) <- widget h
  col <- asks styleColor1
  return (w, drawBox col w h r)
box (FixedHeightWidget widget) = FixedHeightWidget $ \w -> do
  (h, r) <- widget w
  col <- asks styleColor1
  return (h, drawBox col w h r)

drawBox col w h r = do
  setSourceRGB' col
  rectangle 0 0 w h
  stroke
  retain r

-- | A space of variable size
space :: (Applicative f) => CairoWidget (V w) (V h) f
space = FlowWidget $ \w h -> pure (return ())

spaceH :: (Applicative f, Num h) => CairoWidget (V w) (F h) f
spaceH = FixedHeightWidget $ \w -> pure (0, return ())

spaceV :: (Applicative f, Num w) => CairoWidget (F w) (V h) f
spaceV = FixedWidthWidget $ \h -> pure (0, return ())

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
