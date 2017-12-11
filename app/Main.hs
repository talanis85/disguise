module Main where

import Graphics.PUI.Gtk

main :: IO ()
main = do
  (Right img) <- loadImage "test.png"
  let initModel = 0
      updateModel c m | c == 'a'  = m + 1
                      | otherwise = m
      testWidget n = (box (alignleft (text ("Hallo Welt " ++ show n))))
                     `topof` (aligntop (alignleft (fixedImage img)))
  pureMain initModel updateModel testWidget
