{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LaunchPad.Exception
  ( InvalidResponseException (..)
  , InvalidStackStatusException (..)
  , StackNotFoundException (..)
  , SomeLaunchPadException
  ) where

import Control.Exception       (IOException)
import Control.Lens
import Control.Monad.Trans.AWS

import Data.Typeable           (cast)

import GHC.Show                (showsPrec)

import LaunchPad.PrettyPrint

import Relude

data SomeLaunchPadException = forall e. (Exception e, Pretty e) => SomeLaunchPadException e
  deriving Typeable

instance Show SomeLaunchPadException where
  showsPrec p (SomeLaunchPadException e) = showsPrec p e

instance Exception SomeLaunchPadException where

instance Pretty SomeLaunchPadException where
  pretty (SomeLaunchPadException e) = pretty e

newtype InvalidResponseException = InvalidResponseException Text
  deriving (Eq, Pretty, Show)

instance Exception InvalidResponseException where
  toException = toException . SomeLaunchPadException
  fromException sexc = do
    SomeLaunchPadException exc <- fromException sexc
    cast exc

newtype InvalidStackStatusException = InvalidStackStatusException Text
  deriving (Eq, Pretty, Show)

instance Exception InvalidStackStatusException where
  toException = toException . SomeLaunchPadException
  fromException sexc = do
    SomeLaunchPadException exc <- fromException sexc
    cast exc

newtype StackNotFoundException = StackNotFoundException Text
  deriving (Eq, Pretty, Show)

instance Exception StackNotFoundException where
  toException = toException . SomeLaunchPadException
  fromException sexc = do
    SomeLaunchPadException exc <- fromException sexc
    cast exc

instance Pretty IOException where
  pretty = ("ERROR " <>) . show

instance Pretty Error where
  pretty = ("ERROR " <>) . show

instance Pretty ServiceError where
  pretty error = "ERROR "
    <> "ServiceError "
    <> (pretty . view serviceCode) error
    <> ": "
    <> (pretty . view serviceMessage) error

instance Pretty ErrorCode where
  pretty (ErrorCode c) = pretty c

instance Pretty ErrorMessage where
  pretty (ErrorMessage msg) = pretty msg