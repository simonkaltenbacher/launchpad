{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module LaunchPad.Config
  ( Config (..)
  , CI.Param (..)
  , CI.PExpr (..)
  , CI.Stack (..)
  , CI.StackName (..)
  , CI.ResourceId (..)
  , readConfig
  )
  where

import           Control.Monad.Catch             (MonadCatch)
import           Control.Monad.Trans.AWS 
import           Control.Lens.Lens               (lens)
import           Control.Lens.Setter             ((.~))

import           Data.Text                       (Text)

import           GHC.Generics                    (Generic)

import qualified LaunchPad.Config.Internal as CI

import           Network.AWS.S3.Types            (ServerSideEncryption)

import           Path

import           Relude

data Config = Config
  { _env                  :: Env
  , _resourceBucketName   :: Text
  , _sseKmsKeyId          :: Maybe Text
  , _serverSideEncryption :: Maybe ServerSideEncryption
  , _stacks               :: [CI.Stack]
  }
  deriving (Generic)

instance HasEnv Config where
  environment = lens _env (\conf env -> conf {_env = env})

readConfig :: (MonadCatch m, MonadIO m) => Path Abs File -> m Config
readConfig confFile = do
  env <- newEnv Discover <&> envRegion .~ Frankfurt
  CI.DhallConfig {..} <- CI.readDhallConfig confFile
  return Config { _env = env, .. }