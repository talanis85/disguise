{-# LANGUAGE
  RankNTypes
, MultiParamTypeClasses
, GADTs
, TypeFamilies
, TypeSynonymInstances
, FlexibleInstances #-}

module Graphics.PUI.Widget
    ( Widget (..)
    , FlowWidget, FixedWidget, FixedWidthWidget, FixedHeightWidget
    , mkFlow, mkFixed, mkFixedWidth, mkFixedHeight
    , fromFlow, fromFixed, fromFixedWidth, fromFixedHeight
    , hoistWidget
    , fixh, fixw
    , F, V
    , Proxy (Proxy)
    , VarDim (..)
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure, liftA2)
import Control.Monad (liftM)
import Data.Bifunctor
import Data.Proxy
import Data.Traversable (sequenceA)

data F dim
data V dim

class VarDim a where
  type InputOf a :: *
  type OutputOf a :: *
  type ValueOf a :: *

  valueOf :: Proxy a -> InputOf a -> OutputOf a -> ValueOf a

instance VarDim (F dim) where
  type InputOf (F dim) = ()
  type OutputOf (F dim) = dim
  type ValueOf (F dim) = dim

  valueOf _ () dim = dim

instance VarDim (V dim) where
  type InputOf (V dim) = dim
  type OutputOf (V dim) = ()
  type ValueOf (V dim) = dim

  valueOf _ dim () = dim

data Widget w h f a = Widget { fromWidget :: InputOf w -> InputOf h -> f (OutputOf w, OutputOf h, a) }

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

fixh :: (Functor f) => dim -> FlowWidget dim f a -> FixedHeightWidget dim f a
fixh h widget = mkFixedHeight $ \w -> fmap (\r -> (h,r)) ((fromFlow widget) w h)

fixw :: (Functor f) => dim -> FlowWidget dim f a -> FixedWidthWidget dim f a
fixw w widget = mkFixedWidth $ \h -> fmap (\r -> (w,r)) ((fromFlow widget) w h)
