module LaunchPad.Terminal (getConfirmation) where

import Relude

import qualified Streaming.Prelude as S

import           System.IO              (hFlush, stdout)

getConfirmation :: MonadIO m => Int -> (Text -> Maybe Bool) -> Text -> m Bool
getConfirmation numRetry parser msg
    = fmap (fromMaybe False)
    . S.head_
    . S.mapMaybe id
    . S.take numRetry
    . S.repeatM
    $ attempt
  where
    attempt = do
      putText msg
      liftIO $ hFlush stdout
      parser <$> getLine