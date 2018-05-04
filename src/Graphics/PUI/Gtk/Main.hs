{- |
Module      : Graphics.PUI.Gtk.Main
Description : Various main routines for Gtk
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}

module Graphics.PUI.Gtk.Main
  ( ioMain
  , pureMain
  , asyncMain
  , defaultStyle
  , quit
  ) where

import Control.Monad.Trans
import Data.IORef
import Graphics.PUI.Gtk.Event
import Graphics.PUI.Cairo.Widget
import Graphics.PUI.Widget
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G

-- | See 'G.mainQuit'
quit :: IO ()
quit = G.mainQuit

defaultStyle :: IO Style
defaultStyle = do
  font <- G.fontDescriptionFromString "monospace 8"
  return Style
    { styleFont = font
    , styleColor0 = RGB 0 0 0
    , styleColor1 = RGB 1 1 1
    , styleColor2 = RGB 1 0 0
    }

-- | A main function that takes an initial model, a function to transform the model when an event arrives
--   and a function from the model to a widget to display.
ioMain :: Style -> model -> (Event -> model -> IO model) -> (model -> IO (CairoWidget (V Dim) (V Dim) (StyleT IO))) -> IO ()
ioMain style initModel updateModel widget = do
  G.initGUI
  modelRef <- newIORef initModel
  drawingArea <- G.drawingAreaNew
  window <- G.windowNew
  G.containerAdd window drawingArea
  drawingArea `G.on` G.draw $ do
    G.Rectangle x y w h <- liftIO $ G.widgetGetAllocation drawingArea
    C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
    setSourceRGB' (styleColor0 style)
    C.fill
    model <- liftIO $ readIORef modelRef
    widget' <- liftIO $ widget model
    drawit <- liftIO $ drawFlowWidget widget' (fromIntegral w) (fromIntegral h) style
    drawit
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False
  window `G.on` G.keyPressEvent $ do
    keyval <- G.eventKeyVal
    liftIO $ do
      model <- readIORef modelRef
      model' <- updateModel (KeyEvent keyval) model
      writeIORef modelRef model'
      G.widgetQueueDraw drawingArea
      return True
  G.widgetShowAll window
  G.mainGUI

-- | Same as 'ioMain' but without performing IO
pureMain :: Style -> model -> (Event -> model -> model) -> (model -> CairoWidget (V Dim) (V Dim) (StyleT IO)) -> IO ()
pureMain style initModel updateModel widget = ioMain style initModel (fmap (fmap return) updateModel) (return . widget)

-- | For use with typical FRP frameworks
asyncMain :: ((CairoWidget (V Dim) (V Dim) (StyleT IO) -> IO ()) -> IO (Event -> IO ())) -> IO ()
asyncMain init = do
  G.initGUI
  widgetRef <- newIORef Nothing
  style <- defaultStyle
  drawingArea <- G.drawingAreaNew
  window <- G.windowNew
  G.containerAdd window drawingArea
  drawingArea `G.on` G.draw $ do
    widget' <- liftIO $ readIORef widgetRef
    case widget' of
      Nothing -> return ()
      Just widget -> do
        G.Rectangle x y w h <- liftIO $ G.widgetGetAllocation drawingArea
        C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
        setSourceRGB' (styleColor0 style)
        C.fill
        drawit <- liftIO $ drawFlowWidget widget (fromIntegral w) (fromIntegral h) style
        drawit
  window `G.on` G.deleteEvent $ do
    liftIO G.mainQuit
    return False
  handler <- init $ \widget -> G.postGUIAsync $ do
    modifyIORef widgetRef (const (Just widget))
    G.widgetQueueDraw drawingArea
  window `G.on` G.keyPressEvent $ do
    keyval <- G.eventKeyVal
    liftIO $ handler (KeyEvent keyval)
    return True
  G.widgetShowAll window
  G.widgetQueueDraw drawingArea
  G.mainGUI
