{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.PUI.Widget
    ( Widget
    , FlowWidget, FixedWidget, FixedWidthWidget, FixedHeightWidget
    , mkFlow, mkFixed, mkFixedWidth, mkFixedHeight
    , fromFlow, fromFixed, fromFixedWidth, fromFixedHeight
    -- , CompH (horiz), CompV (vert)
    , hoistWidget
    -- , fixh, fixw
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure, liftA2)
import Control.Monad (liftM)
import Data.Traversable (sequenceA)
import Data.Bifunctor

newtype Widget w h fw fh f a = Widget (w -> h -> f (fw, fh, a))

instance (Functor f) => Functor (Widget w h fw fh f) where
  fmap f (Widget w) = Widget (fmap (fmap (fmap (\(a,b,c) -> (a,b,f c)))) w)

type FlowWidget dim        = Widget dim dim () ()
type FixedWidget dim       = Widget () () dim dim
type FixedWidthWidget dim  = Widget () dim dim ()
type FixedHeightWidget dim = Widget dim () () dim

hoistWidget :: (forall a. f a -> g a) -> Widget w h fw fh f a -> Widget w h fw fh g a
hoistWidget f (Widget wid) = Widget $ fmap (fmap f) wid

fromFlow :: (Functor f) => Widget w h () () f a -> (w -> h -> f a)
fromFlow (Widget widget) = \w h -> fmap (\(_, _, x) -> x) (widget w h)

fromFixed :: Widget () () fw fh f a -> f (fw, fh, a)
fromFixed (Widget widget) = widget () ()

fromFixedWidth :: (Functor f) => Widget () h fw () f a -> h -> f (fw, a)
fromFixedWidth (Widget widget) = \h -> fmap (\(w, _, x) -> (w, x)) (widget () h)

fromFixedHeight :: (Functor f) => Widget w () () fh f a -> w -> f (fh, a)
fromFixedHeight (Widget widget) = \w -> fmap (\(_, h, x) -> (h, x)) (widget w ())

mkFlow :: (Functor f) => (w -> h -> f a) -> Widget w h () () f a
mkFlow widget = Widget $ \w h -> fmap (\x -> ((), (), x)) (widget w h)

mkFixed :: f (fw, fh, a) -> Widget () () fw fh f a
mkFixed widget = Widget $ \() () -> widget

mkFixedWidth :: (Functor f) => (h -> f (fw, a)) -> Widget () h fw () f a
mkFixedWidth widget = Widget $ \() h -> fmap (\(w, x) -> (w, (), x)) (widget h)

mkFixedHeight :: (Functor f) => (w -> f (fh, a)) -> Widget w () () fh f a
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
