{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.Deploy (deployStack) where

import Data.ByteString            (readFile)

import Conduit                    (MonadResource)

import Control.Exception          (Exception)

import Control.Lens.Getter        (view)
import Control.Lens.Setter        ((.~), (?~))

import Control.Monad              (liftM, void)
import Control.Monad.Catch        (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.AWS

import Data.Foldable              (find, fold)
import Data.Function              ((&))
import Data.Maybe                 (mapMaybe)
import Data.Text                  (Text, unpack)
import Data.Text.IO               (putStr, putStrLn)

import LaunchPad.Error
import LaunchPad.Type

import Network.AWS.CloudFormation hiding (Stack)
import Network.AWS.S3

import Path

import Prelude                    hiding (putStr, putStrLn, readFile)

import TextShow                   (showt)


type AWSConstraint' m = (MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

data StackNotFoundError = StackNotFoundError Text
  deriving (Eq, Show)

data CreateStackError = CreateStackError Text
  deriving (Eq, Show)

instance Exception StackNotFoundError
instance Exception CreateStackError

deployStack :: AWSConstraint' m => Text -> m StackId
deployStack stackName = do
  stack <- findStack stackName =<< asks _stacks
  liftIO $ putStrLn $ "Uploading templates"
  mapM_ uploadTemplate (listTemplateIds stack)
  templateBucketName <- asks _templateBucketName
  liftIO $ putStrLn $ "Deploying stack " <> stackName <> " to bucket " <> templateBucketName
  performCreateStack stack

performCreateStack :: AWSConstraint' m => Stack -> m StackId
performCreateStack Stack{..} = handleResp =<< handleServiceError . send . createReq =<< asks _templateBucketName
  where
    createReq templateBucketName =
      createStack (_deplEnv <> "-" <> _stackName)
        & csCapabilities    .~ [CapabilityNamedIAM]
        & csDisableRollback ?~ True
        & csParameters      .~ fmap (translateParam templateBucketName) _stackParams
        & csTemplateURL     ?~ genS3Url templateBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ CreateStackError "Received invalid response") (pure . StackId)
      . view csrsStackId

uploadTemplate :: AWSConstraint' m => TemplateId -> m ()
uploadTemplate tid = do
    liftIO $ putStr $ showt tid <> "... "
    templateDir <- asks _templateDir
    templateBucketName <- asks _templateBucketName
    body <- readBody templateDir tid
    send $ putObject
      (BucketName templateBucketName)
      (ObjectKey $ unTemplateId tid)
      body
    liftIO $ putStrLn $ "DONE"
  where
    readBody templateDir
      = liftM (toBody . toHashed)
      . (=<<) (liftIO . readFile . toFilePath)
      . genLocalPath templateDir

findStack :: MonadThrow m => Text -> [Stack] -> m Stack
findStack stackName = maybe (throwM $ err) pure . find ((== stackName) . _stackName)
  where
    err = StackNotFoundError $ "Unable to find Stack " <> stackName <> "."

listTemplateIds :: Stack -> [TemplateId]
listTemplateIds Stack{..} = _stackTemplateId : mapMaybe extract _stackParams
  where
    extract (Param _ (PTemplateId tid)) = pure tid
    extract _                           = Nothing

translateParam :: Text -> Param -> Parameter
translateParam templateBucketName (Param pname pvalue) =
    parameter
      & pParameterKey   ?~ pname
      & pParameterValue ?~ evalParamExpr pvalue
  where
    evalParamExpr (PLit value)      = value
    evalParamExpr (PTemplateId tid) = genS3Url templateBucketName tid

genLocalPath :: MonadThrow m => Path Abs Dir -> TemplateId -> m (Path Abs File)
genLocalPath templateDir = fmap (templateDir </>) . parseRelFile . unpack . unTemplateId

genS3Url :: Text -> TemplateId -> Text
genS3Url templateBucketName tid = fold
  [ "https://"
  , templateBucketName
  , ".s3.eu-central-1.amazonaws.com/"
  , unTemplateId tid
  ]