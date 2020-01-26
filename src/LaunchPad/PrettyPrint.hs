{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LaunchPad.PrettyPrint
  ( getConfirmation
  , getPrettyState
  , initPretty
  , PrettyPrint
  , PrettyState
  , putDocB
  , putDocBLn
  , putTextB
  , putTextBLn
  , renderDoc
  , runPretty
  , withBlock
  , module Data.Text.Prettyprint.Doc
  ) where

import           Control.Lens

import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Relude

import qualified Streaming.Prelude as S

import           System.IO                             (hFlush, stdout)

renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

data PrettyState = PrettyState { _prettyLevel :: Int }
  deriving (Eq, Show)

psPrettyLevel :: Lens' PrettyState Int
psPrettyLevel = lens _prettyLevel (\s l -> s { _prettyLevel = l })

type PrettyPrint m = MonadState PrettyState m

initPretty :: PrettyState
initPretty = PrettyState 0

runPretty :: Monad m => PrettyState -> StateT PrettyState m () -> m ()
runPretty = flip evalStateT

getPrettyState :: PrettyPrint m => m PrettyState
getPrettyState = get

withBlock :: (MonadIO m, PrettyPrint m) => Doc ann -> m () -> m ()
withBlock header block = do
  putDocBLn header
  modify $ over psPrettyLevel (+ 2)
  block
  modify $ over psPrettyLevel (subtract 2)

putDocB :: (MonadIO m, PrettyPrint m) => Doc ann -> m ()
putDocB doc = do
  liftIO . putDoc . flip indent doc . view psPrettyLevel =<< get
  liftIO $ hFlush stdout

putDocBLn :: (MonadIO m, PrettyPrint m) => Doc ann -> m ()
putDocBLn doc = do
  liftIO . putDoc . flip indent doc . view psPrettyLevel =<< get
  putTextLn ""

putTextB :: (MonadIO m, PrettyPrint m) => Text -> m ()
putTextB txt = do 
  putText . (<> txt) . flip T.replicate " " . view psPrettyLevel =<< get
  liftIO $ hFlush stdout

putTextBLn :: (MonadIO m, PrettyPrint m) => Text -> m ()
putTextBLn txt = putTextLn . (<> txt) . flip T.replicate " " . view psPrettyLevel =<< get

getConfirmation :: (MonadIO m, PrettyPrint m) => Int -> (Text -> Maybe Bool) -> Doc ann -> m Bool
getConfirmation numRetry parser msg
    = fmap (fromMaybe False)
    . S.head_
    . S.mapMaybe id
    . S.take numRetry
    . S.repeatM
    $ attempt
  where
    attempt = do
      putDocB msg
      parser <$> getLine
