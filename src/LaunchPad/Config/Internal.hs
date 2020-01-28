{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LaunchPad.Config.Internal
  ( DhallConfig (..)
  , Param (..)
  , PExpr (..)
  , Stack (..)
  , StackName (..)
  , ResourceId (..)
  , readDhallConfig
  )
  where

import Data.Text                 (dropWhile, pack, Text)
import Data.Text.Prettyprint.Doc

import Dhall

import GHC.Generics              (Generic)

import Network.AWS.S3.Types      (ServerSideEncryption)

import Path

import Relude                    hiding (dropWhile)

import Text.Read                 (readsPrec)


data DhallConfig = DhallConfig
  { _resourceBucketName   :: Text
  , _sseKmsKeyId          :: Maybe Text
  , _serverSideEncryption :: Maybe ServerSideEncryption
  , _stacks               :: [Stack]
  }
  deriving (Eq, Generic, Show)

instance FromDhall DhallConfig
instance FromDhall ServerSideEncryption

data Stack = Stack
  { _roleArn         :: Maybe Text
  , _stackName       :: StackName
  , _stackTemplateId :: ResourceId
  , _stackParams     :: [Param]
  }
  deriving (Eq, Generic, Show)

instance FromDhall Stack

data Param = Param
  { _paramName  :: Text
  , _paramValue :: PExpr
  }
  deriving (Eq, Generic, Show)

instance FromDhall Param

data PExpr
  = PLit Text
  | PResourceId ResourceId
  deriving (Eq, Generic, Show)

instance FromDhall PExpr

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Generic, Pretty, Show)

instance FromDhall StackName

instance Read StackName where
  readsPrec p str = [(StackName $ pack str, mempty)]

newtype ResourceId = ResourceId { unResourceId :: Text }
  deriving (Eq, Generic, Pretty, Show)

instance FromDhall ResourceId

readDhallConfig :: MonadIO m => Path Abs File -> m DhallConfig
readDhallConfig = liftIO . inputFile (autoWith interpretOptions) . toFilePath
  where
    interpretOptions = defaultInterpretOptions
      { fieldModifier = dropWhile (== '_')
      , singletonConstructors = Bare
      }