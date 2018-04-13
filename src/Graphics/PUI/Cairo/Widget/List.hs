module Graphics.PUI.Cairo.Widget.List
  ( list
  ) where

import Control.Monad.Reader
import qualified Data.List.Zipper as Z
import Graphics.PUI.Cairo.Widget
import Graphics.PUI.Widget
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

matchZipper :: Z.Zipper a -> ([a], Maybe a, [a])
matchZipper (Z.Zip ls rs) = case rs of
                              []     -> (reverse ls, Nothing, [])
                              (r:rs) -> (reverse ls, Just r, rs)

sliceZipper :: Z.Zipper a -> Int -> ([a], Maybe a, [a])
sliceZipper z n = let (l, c, r) = matchZipper z
                      l' = drop (max 0 (length l - (n `div` 2))) l
                      r' = case c of
                             Nothing -> take (n - length l') r
                             Just _  -> take (n - length l' - 1) r
                  in (l', c, r')

list :: (MonadIO f) => Z.Zipper String -> CairoWidget (V Dim) (V Dim) (StyleT f)
list zipper = FlowWidget $ \w h -> do
  col <- asks styleColor1
  fontdesc <- asks styleFont
  textcolor <- asks styleColor1
  seltextcolor <- asks styleColor2
  context <- liftIO $ cairoCreateContext Nothing
  layout0 <- liftIO $ layoutText context "J"
  liftIO $ layoutSetFontDescription layout0 (Just fontdesc)
  (_, PangoRectangle _ _ _ lineh) <- liftIO $ layoutGetExtents layout0
  let drawit = do
        let itemcount = floor (h / lineh)
            (l, c, r) = sliceZipper zipper itemcount
            drawLine selected str = do
              layout <- liftIO $ layoutText context str
              liftIO $ layoutSetFontDescription layout (Just fontdesc)
              rectangle 0 0 w lineh
              clip
              setSourceRGB' $ if selected then seltextcolor else textcolor
              showLayout layout
              resetClip
              translate 0 lineh
        mapM_ (drawLine False) l
        maybe (return ()) (drawLine True) c
        mapM_ (drawLine False) r
  return drawit
