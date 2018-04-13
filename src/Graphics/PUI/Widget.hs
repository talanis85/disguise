{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module      : Graphics.PUI.Widget
Description : Definition of a widget
Copyright   : Philip Kranz, 2018
License     : GPL-3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Graphics.PUI.Widget
    ( 
      -- * Definition of widgets
      Widget (..)
    , F, V
    , DimOf
    {-
      -- * Functions to create widgets
    , mkFlow, mkFixed, mkFixedWidth, mkFixedHeight
      -- * Functions to match against widgets
    , fromFlow, fromFixed, fromFixedWidth, fromFixedHeight
      -- * Transformations between different flavours
    -}
    , fixh, fixw
      -- * Transformation of the underlying functor
    , hoistWidget
    {-
      -- * Re-exports
    , Proxy (Proxy)
    -}
    ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure, liftA2)
import Control.Monad (liftM)
import Data.Bifunctor
import Data.Proxy
import Data.Traversable (sequenceA)

-- | A fixed size of the given type
data F dim
-- | A variable size of the given type
data V dim

type family DimOf d :: * where
  DimOf (F dim) = dim
  DimOf (V dim) = dim

-- | A widget is a function from up to two dimensions (which denote the requested size of the widget) to the
--   rest of the dimensions (which were left up to the widget to determine, if any) and a value which
--   represents the actual screen representation of the widget.
--
--   If we leave out the type-level machinery here, we can think of it as one of:
--
--   * @ () -> () -> f (w, h, a) @ (fixed width and height)
--   * @ w  -> h  -> f ((), (), a) @ (dynamic width and height)
--   * @ w  -> () -> f ((), h, a) @ (dynamic width and fixed height)
--   * @ () -> h  -> f (w, (), a) @ (fixed width and dynamic height)

-- data Widget w h f a = Widget { fromWidget :: InputOf w -> InputOf h -> f (OutputOf w, OutputOf h, a) }

data Widget w h f a where
  FlowWidget        :: (w -> h -> f a) -> Widget (V w) (V h) f a
  FixedWidget       :: f (w, h, a)     -> Widget (F w) (F h) f a
  FixedWidthWidget  :: (h -> f (w, a)) -> Widget (F w) (V h) f a
  FixedHeightWidget :: (w -> f (h, a)) -> Widget (V w) (F h) f a

deriving instance (Functor f) => Functor (Widget w h f)

-- instance (Functor f) => Functor (Widget w h f) where
--   fmap f (FlowWidget x) = FlowWidget (fmap (fmap (fmap (\(a,b,c) -> (a,b,f c)))) w)

type FlowWidget dim        = Widget (V dim) (V dim)
type FixedWidget dim       = Widget (F dim) (F dim)
type FixedWidthWidget dim  = Widget (F dim) (V dim)
type FixedHeightWidget dim = Widget (V dim) (F dim)

-- | See @hoist@ from the MFunctor package
hoistWidget :: (forall a. f a -> g a) -> Widget w h f a -> Widget w h g a
hoistWidget f (FlowWidget wid) = FlowWidget $ fmap (fmap f) wid
hoistWidget f (FixedWidget wid) = FixedWidget $ f wid
hoistWidget f (FixedWidthWidget wid) = FixedWidthWidget $ fmap f wid
hoistWidget f (FixedHeightWidget wid) = FixedHeightWidget $ fmap f wid

{-
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
-}

-- | Convert a 'FlowWidget' to a 'FixedHeightWidget' by fixing the height to a given value
fixh :: (Functor f) => dim -> Widget (V dim) (V dim) f a -> Widget (V dim) (F dim) f a
fixh h (FlowWidget widget) = FixedHeightWidget $ \w -> fmap (\r -> (h,r)) (widget w h)

-- | Convert a 'FlowWidget' to a 'FixedWidthWidget' by fixing the width to a given value
fixw :: (Functor f) => dim -> Widget (V dim) (V dim) f a -> Widget (F dim) (V dim) f a
fixw w (FlowWidget widget) = FixedWidthWidget $ \h -> fmap (\r -> (w,r)) (widget w h)
