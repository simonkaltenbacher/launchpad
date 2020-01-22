{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LaunchPad.Wait
  ( await
  , CheckResult (..)
  , FailedWaitConditionError
  , MaxNumOfRetriesExceededError
  , RecoverResult (..)
  , WaitCondition (..)
  ) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad.Catch     (throwM)
import           Control.Monad.Trans.AWS hiding (await)

import           Formatting
import           Formatting.Clock        (timeSpecs)

import           Relude

import qualified Streaming.Prelude as S

import           System.Clock
import           System.Console.ANSI     (setCursorColumn)
import           System.IO               (hFlush, stdout)

data CheckResult
  = CheckSuccess
  | CheckRetry
  | CheckFailure Text
  deriving (Eq, Show)

data RecoverResult
  = RecoverRetry
  | RecoverFailure Text
  deriving (Eq, Show)

data WaitCondition a = WaitCondition
  { _check       :: a -> Rs a -> CheckResult
  , _recover     :: Error -> RecoverResult
  , _frequency   :: Int
  , _numAttempts :: Int
  , _waitMessage :: Text
  }

data MaxNumOfRetriesExceededError = MaxNumOfRetriesExceededError Text
  deriving (Eq, Show)

instance Exception MaxNumOfRetriesExceededError

data FailedWaitConditionError = FailedWaitConditionError Text
  deriving (Eq, Show)

instance Exception FailedWaitConditionError

await :: (AWSConstraint r m, AWSRequest a) => WaitCondition a -> a -> m (Rs a)
await WaitCondition{..} req = do
    start <- liftIO $ getTime Monotonic
    stopSig <- liftIO newEmptyMVar 
    conf <- ask
    liftIO . void . forkIO $ runResourceT . runAWST conf $ monitorET stopSig start _waitMessage
    resp <- awaitCond
    liftIO $ putMVar stopSig ()
    putTextLn ""
    return resp
  where
    awaitCond
      = (=<<) (maybe (throwM $ MaxNumOfRetriesExceededError "Maximum number of retries exceeded.") pure)
      . S.head_
      . S.mapMaybe id
      . S.replicateM _numAttempts
      $ poll

    poll = do
      nSecondDelay _frequency
      either recover check =<< trying _Error (send req)

    recover err = case _recover err of
      RecoverRetry         -> pure Nothing
      (RecoverFailure msg) -> throwM $ FailedWaitConditionError msg

    check resp = case _check req resp of
      CheckSuccess       -> return . Just $ resp
      CheckRetry         -> pure Nothing
      (CheckFailure msg) -> throwM $ FailedWaitConditionError msg

monitorET :: MonadIO m => MVar () -> TimeSpec -> Text -> m ()
monitorET stopSig start message = (S.effects . S.untilLeft) printUpdate
  where
    printUpdate = liftIO $ do
      end <- getTime Monotonic
      setCursorColumn 0
      fprint (stext % " - Elapsed time: " % (right 10 ' ' %. timeSpecs)) message start end
      hFlush stdout
      nSecondDelay 1
      maybeToLeft () <$> tryTakeMVar stopSig

nSecondDelay :: MonadIO m => Int -> m ()
nSecondDelay n = liftIO $ threadDelay (n * 1000000)