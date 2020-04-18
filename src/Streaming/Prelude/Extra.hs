module Streaming.Prelude.Extra
  ( maximumOn
  , maximumOn_
  , minimumOn
  , minimumOn_
  , module Streaming.Prelude
  )
  where

import Relude.Custom     hiding (fold)

import Streaming
import Streaming.Prelude

data Maybe_ a = Just_ !a | Nothing_

fromMaybe_ :: Maybe_ a -> Maybe a
fromMaybe_ (Just_ a) = Just a
fromMaybe_ Nothing_  = Nothing

pickArgMax :: Ord b => (a -> b) -> Maybe_ a -> a -> Maybe_ a
pickArgMax f Nothing_ a = Just_ a
pickArgMax f (Just_ a') a
  | f a >= f a' = Just_ a
  | otherwise   = Just_ a'

pickArgMin :: Ord b => (a -> b) -> Maybe_ a -> a -> Maybe_ a
pickArgMin f Nothing_ a = Just_ a
pickArgMin f (Just_ a') a
  | f a <= f a' = Just_ a
  | otherwise   = Just_ a'

maximumOn :: (Monad m, Ord b) => (a -> b) -> Stream (Of a) m r -> m (Of (Maybe a) r)
maximumOn f = fmap (mapOf fromMaybe_) . fold (pickArgMax f) Nothing_ id

maximumOn_ :: (Monad m, Ord b) => (a -> b) -> Stream (Of a) m r -> m (Maybe a)
maximumOn_ f = fmap fromMaybe_ . fold_ (pickArgMax f) Nothing_ id

minimumOn :: (Monad m, Ord b) => (a -> b) -> Stream (Of a) m r -> m (Of (Maybe a) r)
minimumOn f = fmap (mapOf fromMaybe_) . fold (pickArgMin f) Nothing_ id

minimumOn_ :: (Monad m, Ord b) => (a -> b) -> Stream (Of a) m r -> m (Maybe a)
minimumOn_ f = fmap fromMaybe_ . fold_ (pickArgMin f) Nothing_ id
