{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LaunchPad.Type
  ( ChangeSetId (..)
  , ChangeSetIdLike
  , ChangeSetName (..)
  , fromChangeSetId
  , fromChangeSetName
  , fromRawStackId
  , fromRawChangeSetId
  , fromStackId
  , fromStackName
  , Param (..)
  , PExpr (..)
  , Protocol (..)
  , ResourceId (..)
  , Stack (..)
  , StackId (..)
  , StackIdLike
  , StackName (..)
  , unChangeSetIdLike
  , unStackIdLike
  )
  where

import Data.Text                 (pack)
import Data.Text.Prettyprint.Doc

import Dhall

import Relude.Custom

import Text.Read                 (readsPrec)


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

data Protocol = HttpsProtocol | S3Protocol
  deriving (Eq, Generic, Show)

instance FromDhall Protocol

data PExpr
  = PLit Text
  | PResourceId
      { _protocol   :: Protocol
      , _resourceId :: ResourceId
      }
  deriving (Eq, Generic, Show)

instance FromDhall PExpr

newtype StackId = StackId { unStackId :: Text }
  deriving (Eq, Generic, Pretty, Show)

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Generic, Pretty, Show)

instance FromDhall StackName

instance Read StackName where
  readsPrec p str = [(StackName $ pack str, mempty)]

data StackIdLike = StackId' StackId | StackName' StackName
  deriving (Eq, Generic, Show)

fromRawStackId :: Text -> StackIdLike
fromRawStackId = StackId' . StackId

fromStackId :: StackId -> StackIdLike
fromStackId = StackId'

fromStackName :: StackName -> StackIdLike
fromStackName = StackName'

unStackIdLike :: StackIdLike -> Text
unStackIdLike (StackId' stackId) = unStackId stackId
unStackIdLike (StackName' stackName) = unStackName stackName

newtype ResourceId = ResourceId { unResourceId :: Text }
  deriving (Eq, Generic, Pretty, Show)

instance FromDhall ResourceId

newtype ChangeSetId = ChangeSetId { unChangeSetId :: Text }
  deriving (Eq, Generic, Pretty, Show)

newtype ChangeSetName = ChangeSetName { unChangeSetName :: Text }
  deriving (Eq, Generic, Pretty, Show)

data ChangeSetIdLike = ChangeSetId' ChangeSetId | ChangeSetName' ChangeSetName
  deriving (Eq, Generic, Show)

fromChangeSetId :: ChangeSetId -> ChangeSetIdLike
fromChangeSetId = ChangeSetId'

fromChangeSetName :: ChangeSetName -> ChangeSetIdLike
fromChangeSetName = ChangeSetName'

fromRawChangeSetId :: Text -> ChangeSetIdLike
fromRawChangeSetId = ChangeSetId' . ChangeSetId

unChangeSetIdLike :: ChangeSetIdLike -> Text
unChangeSetIdLike (ChangeSetId' csId) = unChangeSetId csId
unChangeSetIdLike (ChangeSetName' csName) = unChangeSetName csName