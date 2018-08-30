{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Graphics.Disguise.Cairo.Widget
Description : Widgets to be displayed with Cairo
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Graphics.Disguise.Cairo.Widget
  ( 
  -- * Definition of Cairo widgets
    Dim
  , CairoWidget
  , StyleT, Style (..), RGB (..)

  -- * Consumption
  , drawFlowWidget
  , runStyleT

  -- * Styling
  , withStyling
  , font, color0, color1, color2
  , loadFont
  , reverseColors, reverseFgBg
  , parseRGB

  -- * Basic layout combinators
  , leftOf, topOf, rightOf, bottomOf
  , tabularH, tabularV
  , alignLeft, alignTop
  , space, spaceH, spaceV
  , stretchH, stretchV
  , fill, box
  , pad
  , clipH, clipV, clip

  -- * Helpers
  , setSourceRGB'
  , retain
  ) where

import Control.Monad.Reader
import Data.Char (digitToInt)
import Data.Functor.Identity
import Data.Monoid
import Graphics.Disguise.Widget
import Graphics.Rendering.Cairo hiding (clip, fill)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Pango

-- | Cairo uses 'Double' for coordinates
type Dim = Double

-- | Color type
data RGB = RGB Double Double Double

parseRGB :: String -> RGB
parseRGB ('#':r1:r2:g1:g2:b1:b2:[]) =
  RGB (hexcomp r1 r2) (hexcomp g1 g2) (hexcomp b1 b2)
    where
      hexcomp c1 c2 = fromIntegral (digitToInt c1 * 16 + digitToInt c2) / 255
parseRGB _ = error "Invalid RGB string"

reverseRGB :: RGB -> RGB
reverseRGB (RGB r g b) = RGB (1 - r) (1 - g) (1 - b)

-- | Style type
data Style = Style
  { styleFont :: FontDescription
  , styleColor0 :: RGB
  , styleColor1 :: RGB
  , styleColor2 :: RGB
  }

type Styling = Endo Style

withStyling :: (Monad f) => Styling -> CairoWidget w h (StyleT f) -> CairoWidget w h (StyleT f)
withStyling styling = hoistWidget (local (appEndo styling))

loadFont :: String -> IO FontDescription
loadFont fontname = liftIO $ fontDescriptionFromString fontname

font :: FontDescription -> Styling
font f = Endo $ \style -> style { styleFont = f }

color0 :: RGB -> Styling
color0 rgb = Endo $ \style -> style { styleColor0 = rgb }

color1 :: RGB -> Styling
color1 rgb = Endo $ \style -> style { styleColor1 = rgb }

color2 :: RGB -> Styling
color2 rgb = Endo $ \style -> style { styleColor2 = rgb }

reverseColors :: Styling
reverseColors = Endo $ \style -> style
  { styleColor0 = reverseRGB (styleColor0 style)
  , styleColor1 = reverseRGB (styleColor1 style)
  , styleColor2 = reverseRGB (styleColor2 style)
  }

reverseFgBg :: Styling
reverseFgBg = Endo $ \style -> style
  { styleColor0 = styleColor1 style
  , styleColor1 = styleColor0 style
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

tabularH :: (Monad f)
         => [(Double, CairoWidget (V Dim) (V Dim) f)] -> CairoWidget (V Dim) (V Dim) f
tabularH [] = error "empty argument to 'tabularH'"
tabularH (x:xs) = case x of
  (_, FlowWidget _) -> tabularFlow (x:xs)
  where
    relativeWidth w (a,b) = clipH (a * w) b
    tabularFlow ws = FlowWidget $ \w h -> do
      let widget' = foldr leftOf space (map (relativeWidth w) ws)
      runFlowWidget widget' w h

tabularV :: (Monad f)
         => [(Double, CairoWidget (V Dim) (V Dim) f)] -> CairoWidget (V Dim) (V Dim) f
tabularV [] = error "empty argument to 'tabularV'"
tabularV (x:xs) = case x of
  (_, FlowWidget _) -> tabularFlow (x:xs)
  where
    relativeHeight h (a,b) = clipV (a * h) b
    tabularFlow ws = FlowWidget $ \w h -> do
      let widget' = foldr topOf space (map (relativeHeight h) ws)
      runFlowWidget widget' w h

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

-- | Fill a widget's background
fill :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim) => CairoWidget w h (StyleT f) -> CairoWidget w h (StyleT f)
fill (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget w h
  col <- asks styleColor0
  return (drawFill col w h r)
fill (FixedWidget widget) = FixedWidget $ do
  (w, h, r) <- widget
  col <- asks styleColor0
  return (w, h, drawFill col w h r)
fill (FixedWidthWidget widget) = FixedWidthWidget $ \h -> do
  (w, r) <- widget h
  col <- asks styleColor0
  return (w, drawFill col w h r)
fill (FixedHeightWidget widget) = FixedHeightWidget $ \w -> do
  (h, r) <- widget w
  col <- asks styleColor0
  return (h, drawFill col w h r)

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

pad :: (Monad f, DimOf w ~ Dim, DimOf h ~ Dim) => Dim -> CairoWidget w h f -> CairoWidget w h f
pad px (FlowWidget widget) = FlowWidget $ \w h -> do
  r <- widget (w - 2 * px) (h - 2 * px)
  return (translate px px >> retain r)
pad px (FixedWidget widget) = FixedWidget $ do
  (w, h, r) <- widget
  return (w + 2 * px, h + 2 * px, translate px px >> retain r)
pad px (FixedWidthWidget widget) = FixedWidthWidget $ \h -> do
  (w, r) <- widget (h - 2 * px)
  return (w + 2 * px, translate px px >> retain r)
pad px (FixedHeightWidget widget) = FixedHeightWidget $ \w -> do
  (h, r) <- widget (w - 2 * px)
  return (h + 2 * px, translate px px >> retain r)

-- | Fix the width of a widget
clipH :: (Monad f, DimOf h ~ Dim) => Dim -> CairoWidget (V Dim) h f -> CairoWidget (F Dim) h f
clipH w (FlowWidget widget) = FixedWidthWidget $ \h -> do
  r <- widget w h
  return (w, retain (rectangle 0 0 w h >> Cairo.clip >> r))
clipH w (FixedHeightWidget widget) = FixedWidget $ do
  (h, r) <- widget w
  return (w, h, retain (rectangle 0 0 w h >> Cairo.clip >> r))

-- | Fix the height of a widget
clipV :: (Monad f, DimOf w ~ Dim) => Dim -> CairoWidget w (V Dim) f -> CairoWidget w (F Dim) f
clipV h (FlowWidget widget) = FixedHeightWidget $ \w -> do
  r <- widget w h
  return (h, retain (rectangle 0 0 w h >> Cairo.clip >> r))
clipV h (FixedWidthWidget widget) = FixedWidget $ do
  (w, r) <- widget h
  return (w, h, retain (rectangle 0 0 w h >> Cairo.clip >> r))

-- | Fix width and height
clip :: (Monad f) => Dim -> Dim -> CairoWidget (V Dim) (V Dim) f -> CairoWidget (F Dim) (F Dim) f
clip w h = clipV h . clipH w

drawBox col w h r = do
  setSourceRGB' col
  rectangle 0 0 w h
  stroke
  retain r

drawFill col w h r = do
  setSourceRGB' col
  rectangle 0 0 w h
  Cairo.fill
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
