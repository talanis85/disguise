module Main where

import Graphics.Disguise.Gtk.Main
import Graphics.Disguise.Cairo
import Graphics.Disguise.Cairo.PDF

main :: IO ()
main = do
  putStrLn "Generating test.pdf"
  renderPdf "test.pdf" 100 20 [ui]

  putStrLn "Displaying GTK UI"
  style <- defaultStyle
  pureMain style () (const id) (const ui)

ui :: CairoWidget (V Dim) (V Dim) (StyleT IO)
ui = alignTop $ box (stretchH (text "FOO")) `leftOf` box (stretchV (text "BAR"))
