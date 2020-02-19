{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.PrettyPrint
  ( getConfirmation
  , getPrettyState
  , initPretty
  , mkTable
  , PrettyPrint
  , PrettyState
  , putDocB
  , putDocBLn
  , putTextB
  , putTextBLn
  , renderDoc
  , reportError
  , reportSuccess
  , runPretty
  , Table
  , tabulate
  , withBlock
  , module Data.Text.Prettyprint.Doc
  , module Data.Text.Prettyprint.Doc.Render.Terminal
  ) where

import           Control.Lens

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Relude
import           Relude.Extra.Foldable1

import qualified Streaming.Prelude as S

import           System.IO                                  (hFlush, stdout)


renderDoc :: Doc AnsiStyle -> Text
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

withBlock :: (MonadIO m, PrettyPrint m) => Doc AnsiStyle -> m () -> m ()
withBlock header block = do
  putDocBLn header
  modify $ over psPrettyLevel (+ 2)
  block
  modify $ over psPrettyLevel (subtract 2)

putDocB :: (MonadIO m, PrettyPrint m) => Doc AnsiStyle -> m ()
putDocB doc = do
  liftIO . putDoc . flip indent doc . view psPrettyLevel =<< get
  liftIO $ hFlush stdout

putDocBLn :: (MonadIO m, PrettyPrint m) => Doc AnsiStyle -> m ()
putDocBLn doc = do
  liftIO . putDoc . flip indent doc . view psPrettyLevel =<< get
  putTextLn ""

putTextB :: (MonadIO m, PrettyPrint m) => Text -> m ()
putTextB txt = do 
  putText . (<> txt) . flip T.replicate " " . view psPrettyLevel =<< get
  liftIO $ hFlush stdout

putTextBLn :: (MonadIO m, PrettyPrint m) => Text -> m ()
putTextBLn txt = putTextLn . (<> txt) . flip T.replicate " " . view psPrettyLevel =<< get

getConfirmation :: (MonadIO m, PrettyPrint m) => Int -> (Text -> Maybe Bool) -> Doc AnsiStyle -> m Bool
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

reportSuccess :: (MonadIO m, PrettyPrint m) => Doc AnsiStyle -> m ()
reportSuccess = putDocBLn . (annotate (color Green) "SUCCESS " <>)

reportError :: MonadIO m => Doc AnsiStyle -> m ()
reportError error = do
  liftIO . putDoc $ annotate (color Red) "ERROR " <> error
  putTextLn ""

newtype Table = Table { _rows :: NonEmpty (NonEmpty (Doc AnsiStyle)) }

mkTable :: [[Doc AnsiStyle]] -> Table
mkTable = Table . NE.fromList . fmap NE.fromList

tabulate :: Table -> Doc AnsiStyle
tabulate Table{..} = rows <> line
  where
    rows
      = vsep
      . NE.toList
      . fmap (joinCells . padCells)
      $ _rows

    colWidths
      = fmap (maximum1 . fmap (T.length . renderDoc))
      . NE.transpose
      $ _rows

    padCells = fmap (uncurry fill) . NE.zip colWidths

    joinCells = concatWith (surround "  ")