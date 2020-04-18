module Relude.Custom
  ( liftD
  , module Relude
  )
  where

import Relude

liftD :: Monad m => (a -> m b) -> m a -> m b
liftD = (=<<)