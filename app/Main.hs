{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Graphics.PUI.Cairo
import Graphics.PUI.Gtk.Event
import Graphics.PUI.Gtk.Main

main :: IO ()
main = do
  (Right img) <- loadImage "test.png"
  let initModel = 0
      -- updateModel c m | c == 'a'  = m + 1
      --                | otherwise = m
      updateModel (KeyEvent key) m = putStrLn (keyName key) >> return m
      testWidget n = return $
                (box (alignLeft (text ("Hallo Welt " ++ show n))))
        `topOf` (alignTop (alignLeft (fixedImage img)))
  ioMain initModel updateModel testWidget

{-
topofI = liftA2 topof

main :: IO ()
main = do
  (Right img) <- loadImage "test.png"
  let initModel = 0 :: Int
      testWidget =
                (box <$> alignleft <$> (bindKeyName "a" (+1) (modelText show)))
        `topofI` (aligntop <$> alignleft <$> pure (fixedImage img))
  interactiveMain initModel testWidget

modelText :: (model -> String) -> Interactive (State model e (CairoWidget (F Dim) (F Dim) (StyleT IO))
modelText f = interactive (mkInteractive view trans ())
  where
    view () model = text (f model)
    trans () e = Nothing

bindKeyName :: String -> (model -> model) -> Interactive' model Event (CairoWidget w h m) -> Interactive' model Event (CairoWidget w h m)
bindKeyName keyname modelf i = interactive (mkInteractive view trans (getInteractive' i))
  where
    view i model = extract i model
    trans i e = case e of
                  KeyEvent keyval -> if keyName keyval == keyname
                                        then Just $ modify modelf >> return i
                                        else step e i

countertext :: Interactive' Int Event (CairoWidget (F Dim) (F Dim) (StyleT IO))
countertext = interactive (mkInteractive view trans ())
  where
    view () model = text (show model)
    trans () e = case e of
                   KeyEvent keyval -> if keyName keyval == "a"
                                         then Just $ modify (+ 1) >> return ()
                                         else Nothing
-}
