module Main where

import Graphics.PUI.Gtk.Test
import Graphics.PUI.Gtk.Widget
import Graphics.PUI.Gtk.Image

main :: IO ()
main = do
  (Right img) <- loadImage "test.png"
  let initModel = 0
      updateModel c m | c == 'a'  = m + 1
                      | otherwise = m
      testWidget n = (box (alignleft (text ("Hallo Welt " ++ show n))))
                     `topof` (aligntop (alignleft (fixedImage img)))
  testWindow initModel updateModel testWidget
