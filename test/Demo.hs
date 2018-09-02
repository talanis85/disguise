module Main where

import Graphics.Disguise.Gtk.Main
import Graphics.Disguise.Cairo

main :: IO ()
main = do
  style <- defaultStyle
  pureMain style () (const id) (const ui)

ui :: CairoWidget (V Dim) (V Dim) (StyleT IO)
ui = alignTop $ box (stretchH (text "FOO")) `leftOf` box (stretchV (text "BAR"))
