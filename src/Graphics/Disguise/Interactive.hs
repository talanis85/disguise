module Graphics.Disguise.Interactive
  ( Interactive
  , step
  , mkInteractive
  , extract
  ) where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Graphics.Disguise.Widget

newtype IF m e a = IF { getIF :: e -> Maybe (m a) }

instance (Functor m) => Functor (IF m e) where
  fmap f (IF x) = IF (fmap (fmap (fmap f)) x)

instance (Applicative m) => Applicative (IF m e) where
  pure x = IF $ const $ Just $ pure x
  f <*> p = IF $ \x -> case (getIF f x, getIF p x) of
                           (Nothing, _      ) -> Nothing
                           (_,       Nothing) -> Nothing
                           (Just a,  Just b ) -> Just $ liftA2 ($) a b

instance (Applicative m) => Alternative (IF m e) where
  empty = IF $ const Nothing
  a <|> b = IF $ \x -> case (getIF a x, getIF b x) of
                         (Nothing, x) -> x
                         (x, y)       -> x

type Interactive m e = Cofree (IF m e)

step :: (Functor m) => e -> Interactive m e a -> Maybe (m (Interactive m e a))
step k i = getIF (unwrap i) k

mkInteractive fc ft s = unfold (\x -> (fc x, IF (ft x))) s
