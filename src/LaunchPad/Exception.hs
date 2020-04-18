{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LaunchPad.Exception
  ( FailedWaitConditionError (..)
  , InvalidResponseException (..)
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

import Relude.Custom

data SomeLaunchPadException = forall e. (Exception e, Pretty e) => SomeLaunchPadException e
  deriving Typeable

instance Show SomeLaunchPadException where
  showsPrec p (SomeLaunchPadException e) = showsPrec p e

instance Exception SomeLaunchPadException where
  toException = toException . SomeException
  fromException sexc = do
    SomeException exc <- fromException sexc
    cast exc

instance Pretty SomeLaunchPadException where
  pretty (SomeLaunchPadException e) = pretty e

newtype FailedWaitConditionError = FailedWaitConditionError Text
  deriving (Eq, Pretty, Show)

instance Exception FailedWaitConditionError where
  toException = toException . SomeLaunchPadException
  fromException sexc = do
    SomeLaunchPadException exc <- fromException sexc
    cast exc

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
  pretty = show

instance Pretty Error where
  pretty = show

instance Pretty ServiceError where
  pretty error = "ServiceError "
    <> (pretty . view serviceCode) error
    <> ": "
    <> (pretty . view serviceMessage) error

instance Pretty ErrorCode where
  pretty (ErrorCode c) = pretty c

instance Pretty ErrorMessage where
  pretty (ErrorMessage msg) = pretty msg