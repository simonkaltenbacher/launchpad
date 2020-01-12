{-# LANGUAGE DeriveGeneric #-}

module LaunchPad.Type
  ( Config (..)
  , Param (..)
  , PExpr (..)
  , Stack (..)
  , StackId (.. )
  , TemplateId (..)
  )
  where

import Control.Lens.Lens       (lens)

import Control.Monad.Trans.AWS

import Data.Text               (Text)

import Dhall

import GHC.Generics            (Generic)

import Path

data Config = Config
  { _env                :: Env
  , _templateBucketName :: Text
  , _templateDir        :: Path Abs Dir
  , _stacks             :: [Stack]
  }

instance HasEnv Config where
  environment = lens _env (\conf env -> conf {_env = env})

data Stack = Stack
  { _deplEnv         :: Text
  , _stackName       :: Text
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

newtype TemplateId = TemplateId { unTemplateId :: Text }
  deriving (Eq, Generic, Show)

instance FromDhall TemplateId

newtype StackId = StackId { unStackId :: Text }
  deriving (Eq, Generic, Show)