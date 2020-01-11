{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.Deploy (deployStack) where

import           Data.ByteString            (readFile)

import           Conduit                    (MonadResource)

import           Control.Exception          (Exception)

import           Control.Lens.Getter        (view)
import           Control.Lens.Setter        ((.~), (?~))

import           Control.Monad              (liftM, void)
import           Control.Monad.Catch        (MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS

import           Data.Foldable              (find, fold)
import           Data.Function              ((&))
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (pack, Text, unpack)

import           LaunchPad.Type             (Config, Param, PExpr (..), Stack, StackId (..), TemplateId (..))
import qualified LaunchPad.Type as T

import           Network.AWS.CloudFormation hiding (Stack)
import           Network.AWS.S3

import           Path

import           Prelude                    hiding (readFile)


type AWSConstraint' m = (MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

data StackNotFoundError = StackNotFoundError String
  deriving (Eq, Show)

data CreateStackError = CreateStackError String
  deriving (Eq, Show)

instance Exception StackNotFoundError
instance Exception CreateStackError

deployStack :: AWSConstraint' m => Text -> m StackId
deployStack stackName = do
  stack <- findStack stackName =<< asks T.stacks
  liftIO $ putStrLn $ "Uploading templates"
  mapM_ uploadTemplate (listTemplateIds stack)
  performCreateStack stack

performCreateStack :: AWSConstraint' m => Stack -> m StackId
performCreateStack T.Stack{..} = handleResp =<< send . createReq =<< asks T.templateBucketName
  where
    createReq templateBucketName =
      createStack (deplEnv <> "-" <> stackName)
        & csCapabilities    .~ [CapabilityNamedIAM]
        & csDisableRollback ?~ True
        & csParameters      .~ fmap (translateParam templateBucketName) stackParams
        & csTemplateURL     ?~ genS3Url templateBucketName stackTemplateId

    handleResp resp =
      let rcode = (view csrsResponseStatus) resp
      in
        if rcode == 200
        then maybe
          (throwM $ CreateStackError "Received invalid response")
          (pure . StackId)
          (view csrsStackId $ resp)
        else
          throwM $ CreateStackError ("Received http status " <> show rcode)

uploadTemplate :: AWSConstraint' m => TemplateId -> m ()
uploadTemplate tid = do
    liftIO $ putStr $ show (unTemplateId tid) <> "... "
    templateDir <- asks T.templateDir
    templateBucketName <- asks T.templateBucketName
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
findStack stackName = maybe (throwM $ err) pure . find ((== stackName) . T.stackName)
  where
    err = StackNotFoundError $ "Unable to find Stack " <> unpack stackName <> "."

listTemplateIds :: Stack -> [TemplateId]
listTemplateIds T.Stack{..} = stackTemplateId : mapMaybe extract stackParams
  where
    extract (T.Param _ (PTemplateId tid)) = pure tid
    extract _                           = Nothing

translateParam :: Text -> Param -> Parameter
translateParam templateBucketName (T.Param pname pvalue) =
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