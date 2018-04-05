{- |
Module      : Graphics.PUI.Gtk.Event
Description : Events
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Graphics.PUI.Gtk.Event
  ( Event (..)
  , keyName
  , keyChar
  ) where

import qualified Graphics.UI.Gtk as Gtk
import qualified Data.Text as T

-- | We only support key events for now
data Event = KeyEvent Gtk.KeyVal

keyName :: Gtk.KeyVal -> String
keyName = T.unpack . Gtk.keyName

keyChar :: Gtk.KeyVal -> Maybe Char
keyChar = Gtk.keyToChar
