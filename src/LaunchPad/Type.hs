{-# LANGUAGE DeriveGeneric #-}

module LaunchPad.Type
  ( Config (..)
  , Param (..)
  , PExpr (..)
  , Stack (..)
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
  deriving (Eq, Generic, Show)

instance FromDhall Stack

data Param = Param Text PExpr
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