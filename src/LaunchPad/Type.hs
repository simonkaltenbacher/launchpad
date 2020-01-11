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

import TextShow                (TextShow (..))

data Config = Config
  { env                :: Env
  , templateBucketName :: Text
  , templateDir        :: Path Abs Dir
  , stacks             :: [Stack]
  }

instance HasEnv Config where
  environment = lens env (\conf env -> conf {env = env})

data Stack = Stack
  { deplEnv         :: Text
  , stackName       :: Text
  , stackTemplateId :: TemplateId
  , stackParams     :: [Param]
  }
  deriving (Eq, Generic)

instance FromDhall Stack

data Param = Param
  { paramName  :: Text
  , paramValue :: PExpr
  }
  deriving (Eq, Generic)

instance FromDhall Param

data PExpr
  = PLit Text
  | PTemplateId TemplateId
  deriving (Eq, Generic)

instance FromDhall PExpr

newtype TemplateId = TemplateId { unTemplateId :: Text }
  deriving (Eq, Generic)

instance FromDhall TemplateId

instance TextShow TemplateId where
  showb = showb . unTemplateId

newtype StackId = StackId { unStackId :: Text }
  deriving (Eq, Generic)

instance TextShow StackId where
  showb = showb . unStackId