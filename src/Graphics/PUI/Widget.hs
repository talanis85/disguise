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
      -- * Transformations between different flavours
    , fixh, fixw
      -- * Transformation of the underlying functor
    , hoistWidget
    ) where

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
data Widget w h f a where
  FlowWidget        :: (w -> h -> f a) -> Widget (V w) (V h) f a
  FixedWidget       :: f (w, h, a)     -> Widget (F w) (F h) f a
  FixedWidthWidget  :: (h -> f (w, a)) -> Widget (F w) (V h) f a
  FixedHeightWidget :: (w -> f (h, a)) -> Widget (V w) (F h) f a

deriving instance (Functor f) => Functor (Widget w h f)

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

-- | Convert a 'FlowWidget' to a 'FixedHeightWidget' by fixing the height to a given value
fixh :: (Functor f) => dim -> Widget (V dim) (V dim) f a -> Widget (V dim) (F dim) f a
fixh h (FlowWidget widget) = FixedHeightWidget $ \w -> fmap (\r -> (h,r)) (widget w h)

-- | Convert a 'FlowWidget' to a 'FixedWidthWidget' by fixing the width to a given value
fixw :: (Functor f) => dim -> Widget (V dim) (V dim) f a -> Widget (F dim) (V dim) f a
fixw w (FlowWidget widget) = FixedWidthWidget $ \h -> fmap (\r -> (w,r)) (widget w h)
