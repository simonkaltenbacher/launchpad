{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LaunchPad.Config.Internal
  ( DhallConfig (..)
  , readDhallConfig
  )
  where

import Data.Text                 (dropWhile)

import Dhall

import Network.AWS.S3.Types      (ServerSideEncryption)

import LaunchPad.Type

import Path

import Relude.Custom             hiding (dropWhile)


data DhallConfig = DhallConfig
  { _resourceBucketName   :: Text
  , _sseKmsKeyId          :: Maybe Text
  , _serverSideEncryption :: Maybe ServerSideEncryption
  , _stacks               :: [Stack]
  }
  deriving (Eq, Generic, Show)

instance FromDhall DhallConfig
instance FromDhall ServerSideEncryption

readDhallConfig :: MonadIO m => Path Abs File -> m DhallConfig
readDhallConfig = liftIO . inputFile (autoWith interpretOptions) . toFilePath
  where
    interpretOptions = defaultInterpretOptions
      { fieldModifier = dropWhile (== '_')
      , singletonConstructors = Bare
      }