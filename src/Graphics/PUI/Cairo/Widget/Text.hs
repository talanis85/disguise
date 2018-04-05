module Graphics.PUI.Cairo.Widget.Text
  ( text
  , editText
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Graphics.PUI.Cairo.Widget
import Graphics.PUI.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

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

editText :: (MonadIO f) => String -> Bool -> Int -> CairoWidget (F Dim) (F Dim) (StyleT f)
editText str editing cursor = mkFixed $ do
  fontdesc <- asks styleFont
  textcolor <- asks $ if editing then styleColor2 else styleColor1
  context <- liftIO $ cairoCreateContext Nothing
  layout <- liftIO $ if cursor >= length str
                        then layoutText context (str ++ "_")
                        else layoutText context (insertCursorMarkup cursor str)
  liftIO $ layoutSetFontDescription layout (Just fontdesc)
  (_, PangoRectangle x y w h) <- liftIO $ layoutGetExtents layout
  let drawit = do
        setSourceRGB' textcolor
        showLayout layout
  return (w, h, drawit)

insertCursorMarkup 0 (x:xs) = "<u>" ++ [x] ++ "</u>" ++ xs
insertCursorMarkup n (x:xs) = x : insertCursorMarkup (n-1) xs
insertCursorMarkup n [] = []
