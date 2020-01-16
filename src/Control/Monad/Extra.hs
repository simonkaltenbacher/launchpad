module Control.Monad.Extra (eitherM) where

import Relude

eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM l r x = either l r =<< x