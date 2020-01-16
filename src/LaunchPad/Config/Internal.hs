{-# LANGUAGE DeriveGeneric #-}

module LaunchPad.Config.Internal
  ( DhallConfig (..)
  , Param (..)
  , PExpr (..)
  , Stack (..)
  , StackName (..)
  , TemplateId (..)
  , readDhallConfig
  )
  where

import Data.Text    (dropWhile, pack, Text)

import Dhall

import GHC.Generics (Generic)

import Path

import Relude       hiding (dropWhile)

import Text.Read    (readsPrec)

data DhallConfig = DhallConfig
  { _templateBucketName :: Text
  , _stacks             :: [Stack]
  }
  deriving (Eq, Generic, Show)

instance FromDhall DhallConfig

data Stack = Stack
  { _deplEnv         :: Text
  , _stackName       :: StackName
  , _stackTemplateId :: TemplateId
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
  | PTemplateId TemplateId
  deriving (Eq, Generic, Show)

instance FromDhall PExpr

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Generic, Show)

instance FromDhall StackName

instance Read StackName where
  readsPrec p str = [(StackName $ pack str, mempty)]

newtype TemplateId = TemplateId { unTemplateId :: Text }
  deriving (Eq, Generic, Show)

instance FromDhall TemplateId

readDhallConfig :: MonadIO m => Path Abs File -> m DhallConfig
readDhallConfig = liftIO . inputFile (autoWith interpretOptions) . toFilePath
  where
    interpretOptions = defaultInterpretOptions
      { fieldModifier = dropWhile (== '_')
      , singletonConstructors = Bare
      }