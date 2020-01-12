module LaunchPad.Error
  ( handleServiceError
  , ServiceErrorReport
  ) where

import Control.Exception       (Exception)

import Control.Lens.Getter     (view)

import Control.Monad.Catch     (MonadCatch, MonadThrow, throwM)
import Control.Monad.Trans.AWS (catching)

import Network.AWS

import Data.Text               (Text, unpack)


data ServiceErrorReport = ServiceErrorReport { unServiceErrorReport :: ServiceError }
  deriving Eq

instance Exception ServiceErrorReport

instance Show ServiceErrorReport where
  showsPrec d (ServiceErrorReport error)
      = showString "ServiceError "
      . showString (extractErrorCode error)
      . showString ": "
      . showString (extractErrorMessage error)
    where
      extractErrorCode = unpack . (\(ErrorCode c) -> c) . view serviceCode
      extractErrorMessage = unpack . foldMap (\(ErrorMessage m) -> m) . view serviceMessage

handleServiceError :: (MonadCatch m, MonadThrow m) => m a -> m a
handleServiceError = flip (catching _ServiceError) (throwM . ServiceErrorReport) 