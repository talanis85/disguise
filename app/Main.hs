module Main where

import Graphics.PUI.Gtk.Test
import Graphics.PUI.Gtk.Widget

main :: IO ()
main = testWindow testWidget

testWidget = (box (text "Hallo Welt\nfoobar") `leftof` fill) `topof` fill
