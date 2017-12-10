module Main where

import Graphics.PUI.Gtk.Test
import Graphics.PUI.Gtk.Widget

main :: IO ()
main = testWindow initModel updateModel testWidget
  where
    initModel = 0
    updateModel c m | c == 'a'  = m + 1
                    | otherwise = m
    testWidget n = aligntop (box (alignleft (text ("Hallo Welt " ++ show n))))
