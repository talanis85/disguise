{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Module      : Disguise.Widget
Description : Definition of a widget
Copyright   : Philip Kranz, 2018
License     : BSD3
Maintainer  : pk@pmlk.net
Stability   : experimental
-}
module Disguise.Widget
    ( 
      -- * Definition of widgets
      Widget (..)
    , F, V
    , DimOf
    , runFixedWidget
    , runFlowWidget
    , runFixedWidthWidget
    , runFixedHeightWidget
      -- * bind for widgets
    , BindWidget (..)
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

-- | Use a monadic action in a widget.
class BindWidget w where
  bindWidget :: (Monad f) => (a -> w f b) -> f a -> w f b

instance BindWidget (Widget (V w) (V h)) where
  bindWidget f x = FlowWidget $ \w h -> do
    x' <- x
    let FlowWidget g = f x' in g w h

instance BindWidget (Widget (F w) (F h)) where
  bindWidget f x = FixedWidget $ do
    x' <- x
    let FixedWidget g = f x' in g

instance BindWidget (Widget (V w) (F h)) where
  bindWidget f x = FixedHeightWidget $ \w -> do
    x' <- x
    let FixedHeightWidget g = f x' in g w

instance BindWidget (Widget (F w) (V h)) where
  bindWidget f x = FixedWidthWidget $ \h -> do
    x' <- x
    let FixedWidthWidget g = f x' in g h

type FlowWidget dim        = Widget (V dim) (V dim)
type FixedWidget dim       = Widget (F dim) (F dim)
type FixedWidthWidget dim  = Widget (F dim) (V dim)
type FixedHeightWidget dim = Widget (V dim) (F dim)

runFlowWidget :: FlowWidget dim f a -> dim -> dim -> f a
runFlowWidget (FlowWidget f) w h = f w h

runFixedWidget :: FixedWidget dim f a -> f (dim, dim, a)
runFixedWidget (FixedWidget r) = r

runFixedWidthWidget :: FixedWidthWidget dim f a -> dim -> f (dim, a)
runFixedWidthWidget (FixedWidthWidget f) h = f h

runFixedHeightWidget :: FixedHeightWidget dim f a -> dim -> f (dim, a)
runFixedHeightWidget (FixedHeightWidget f) w = f w

-- | See @hoist@ from the MFunctor package
hoistWidget :: (forall a. f a -> g a) -> Widget w h f a -> Widget w h g a
hoistWidget f (FlowWidget wid) = FlowWidget $ fmap (fmap f) wid
hoistWidget f (FixedWidget wid) = FixedWidget $ f wid
hoistWidget f (FixedWidthWidget wid) = FixedWidthWidget $ fmap f wid
hoistWidget f (FixedHeightWidget wid) = FixedHeightWidget $ fmap f wid

-- | Convert a 'FlowWidget' to a 'FixedHeightWidget' by fixing the height to a given value
fixh :: (Functor f) => dim -> Widget w (V dim) f a -> Widget w (F dim) f a
fixh h (FlowWidget widget) = FixedHeightWidget $ \w -> fmap (\r -> (h,r)) (widget w h)
fixh h (FixedWidthWidget widget) = FixedWidget $ fmap (\(w,r) -> (w,h,r)) (widget h)

-- | Convert a 'FlowWidget' to a 'FixedWidthWidget' by fixing the width to a given value
fixw :: (Functor f) => dim -> Widget (V dim) h f a -> Widget (F dim) h f a
fixw w (FlowWidget widget) = FixedWidthWidget $ \h -> fmap (\r -> (w,r)) (widget w h)
fixw w (FixedHeightWidget widget) = FixedWidget $ fmap (\(h,r) -> (w,h,r)) (widget w)
