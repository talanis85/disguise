{-# LANGUAGE
  RankNTypes
, MultiParamTypeClasses
, TypeFamilies
, TypeSynonymInstances
, FlexibleInstances #-}

module Graphics.PUI.Widget
    ( Widget (..)
    , FlowWidget, FixedWidget, FixedWidthWidget, FixedHeightWidget
    , mkFlow, mkFixed, mkFixedWidth, mkFixedHeight
    , fromFlow, fromFixed, fromFixedWidth, fromFixedHeight
    -- , CompH (horiz), CompV (vert)
    , hoistWidget
    -- , fixh, fixw
    , F, V, InputOf, OutputOf, ValueOf
    , WidgetSize (..)
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure, liftA2)
import Control.Monad (liftM)
import Data.Traversable (sequenceA)
import Data.Bifunctor

data F dim
data V dim

type family InputOf (a :: *) where
  InputOf (F x) = ()
  InputOf (V x) = x

type family OutputOf (a :: *) where
  OutputOf (F x) = x
  OutputOf (V x) = ()

type family ValueOf (a :: *) where
  ValueOf (F x) = x
  ValueOf (V x) = x

class WidgetSize w h where
  widgetSize :: (Functor f) => InputOf w -> InputOf h -> Widget w h f a -> f (ValueOf w, ValueOf h, OutputOf w, OutputOf h)

instance WidgetSize (F w) (F h) where
  widgetSize () () widget = fmap (\(w, h, _) -> (w, h, w, h)) (fromFixed widget)

instance WidgetSize (F w) (V h) where
  widgetSize () h widget = fmap (\(w, _) -> (w, h, w, ())) (fromFixedWidth widget h)

instance WidgetSize (V w) (F h) where
  widgetSize w () widget = fmap (\(h, _) -> (w, h, (), h)) (fromFixedHeight widget w)

instance WidgetSize (V w) (V h) where
  widgetSize w h widget = fmap (\_ -> (w, h, (), ())) (fromFlow widget w h)

newtype Widget w h f a = Widget { fromWidget :: InputOf w -> InputOf h -> f (OutputOf w, OutputOf h, a) }

instance (Functor f) => Functor (Widget w h f) where
  fmap f (Widget w) = Widget (fmap (fmap (fmap (\(a,b,c) -> (a,b,f c)))) w)

type FlowWidget dim        = Widget (V dim) (V dim)
type FixedWidget dim       = Widget (F dim) (F dim)
type FixedWidthWidget dim  = Widget (F dim) (V dim)
type FixedHeightWidget dim = Widget (V dim) (F dim)

hoistWidget :: (forall a. f a -> g a) -> Widget w h f a -> Widget w h g a
hoistWidget f (Widget wid) = Widget $ fmap (fmap f) wid

fromFlow :: (Functor f) => Widget (V w) (V h) f a -> (w -> h -> f a)
fromFlow (Widget widget) = \w h -> fmap (\(_, _, x) -> x) (widget w h)

fromFixed :: Widget (F w) (F h) f a -> f (w, h, a)
fromFixed (Widget widget) = widget () ()

fromFixedWidth :: (Functor f) => Widget (F w) (V h) f a -> h -> f (w, a)
fromFixedWidth (Widget widget) = \h -> fmap (\(w, _, x) -> (w, x)) (widget () h)

fromFixedHeight :: (Functor f) => Widget (V w) (F h) f a -> w -> f (h, a)
fromFixedHeight (Widget widget) = \w -> fmap (\(_, h, x) -> (h, x)) (widget w ())

mkFlow :: (Functor f) => (w -> h -> f a) -> Widget (V w) (V h) f a
mkFlow widget = Widget $ \w h -> fmap (\x -> ((), (), x)) (widget w h)

mkFixed :: f (w, h, a) -> Widget (F w) (F h) f a
mkFixed widget = Widget $ \() () -> widget

mkFixedWidth :: (Functor f) => (h -> f (w, a)) -> Widget (F w) (V h) f a
mkFixedWidth widget = Widget $ \() h -> fmap (\(w, x) -> (w, (), x)) (widget h)

mkFixedHeight :: (Functor f) => (w -> f (h, a)) -> Widget (V w) (F h) f a
mkFixedHeight widget = Widget $ \w () -> fmap (\(h, x) -> ((), h, x)) (widget w)

{-
class CompH w1 w2 w3 where
    horiz :: (Monad f, Applicative f) => w1 f -> w2 f -> w3 f

class CompV w1 w2 w3 where
    vert  :: (Monad f, Applicative f) => w1 f -> w2 f -> w3 f
-}

{-
fixh :: (Functor f) => dim -> FlowWidget dim f a -> FixedHeightWidget dim f a
fixh h widget = mkFixedHeight $ \w -> fmap (\(c,i) -> (h,(c,i))) ((fromFlow widget) w h)

fixw :: (Functor f) => dim -> FlowWidget dim f a -> FixedWidthWidget dim f a
fixw w widget = mkFixedWidth $ \h -> fmap (\(c,i) -> (w,(c,i))) ((fromFlow widget) w h)
-}
