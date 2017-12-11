module Graphics.PUI.Gtk.Test
  ( testWindow
  ) where

import Control.Monad.Trans
import Data.IORef
import Graphics.PUI.Gtk.Widget
import Graphics.PUI.Widget
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G

testWindow :: model -> (Char -> model -> model) -> (model -> CairoWidget (V Dim) (V Dim) (StyleT IO)) -> IO ()
testWindow initModel updateModel widget = do
  G.initGUI
  modelRef <- newIORef initModel
  font <- G.fontDescriptionFromString "monospace 8"
  let style = Style
        { styleFont = font
        , styleColor0 = RGB 0 0 0
        , styleColor1 = RGB 1 1 1
        }
  drawingArea <- G.drawingAreaNew
  window <- G.windowNew
  G.containerAdd window drawingArea
  drawingArea `G.on` G.draw $ do
    G.Rectangle x y w h <- liftIO $ G.widgetGetAllocation drawingArea
    C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
    setSourceRGB' (styleColor0 style)
    C.fill
    model <- liftIO $ readIORef modelRef
    drawit <- liftIO $ drawFlowWidget (widget model) (fromIntegral w) (fromIntegral h) style
    drawit
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False
  window `G.on` G.keyPressEvent $ do
    keyval <- G.eventKeyVal
    let keychar = G.keyToChar keyval
    case keychar of
      Nothing -> return False
      Just c  -> do
        liftIO $ modifyIORef modelRef (updateModel c)
        liftIO $ G.widgetQueueDraw drawingArea
        return True
  G.widgetShowAll window
  G.mainGUI
