{-# LANGUAGE DeriveGeneric #-}

module LaunchPad.Type.Dhall (DhallConfig (..)) where

import LaunchPad.Type hiding (Config)

import Data.Text      (Text)

import Dhall

import GHC.Generics   (Generic)

data DhallConfig = DhallConfig
  { templateBucketName :: Text
  , stacks             :: [Stack]
  }
  deriving (Eq, Generic, Show)

instance FromDhall DhallConfig