{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.Deploy (deployStack) where

import           Data.ByteString            (readFile)

import           Conduit                    (MonadResource)

import           Control.Lens.Getter        (view)
import           Control.Lens.Setter        ((.~), (?~))

import           Control.Monad              (liftM, void)
import           Control.Monad.Catch        (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS

import           Data.Foldable              (fold)
import           Data.Function              ((&))
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (pack, Text, unpack)

import           LaunchPad.Type             (Config, Stack, Param (..), PExpr (..), TemplateId (..))
import qualified LaunchPad.Type as T

import           Network.AWS.CloudFormation hiding (Stack)
import           Network.AWS.S3

import           Path

import           Prelude                    hiding (readFile)


type AWSConstraint' m = (MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

deployStack :: AWSConstraint' m => Text -> m ()
deployStack stackName = undefined

  {-
  runResourceT . runAWST env $ do
  mapM_ uploadTemplate (listTemplateIds stack)
  performCreateStack stack
  -}

performCreateStack :: AWSConstraint' m => Stack -> m ()
performCreateStack T.Stack{..} = do
  templateBucketName <- asks T.templateBucketName
  void . send $ createStack (deplEnv <> "-" <> stackName)
    & csCapabilities    .~ [CapabilityNamedIAM]
    & csDisableRollback ?~ True
    & csParameters      .~ fmap (translateParam templateBucketName) stackParams
    & csTemplateURL     ?~ genS3Url templateBucketName stackTemplateId

uploadTemplate :: AWSConstraint' m => TemplateId -> m ()
uploadTemplate tid = do
    templateDir <- asks T.templateDir
    templateBucketName <- asks T.templateBucketName
    body <- readBody templateDir tid
    send $ putObject
      (BucketName templateBucketName)
      (ObjectKey $ unTemplateId tid)
      body
    liftIO $ putStrLn $ "Uploaded template " <> show (unTemplateId tid)
  where
    readBody templateDir
      = liftM (toBody . toHashed)
      . (=<<) (liftIO . readFile . toFilePath)
      . genLocalPath templateDir

listTemplateIds :: Stack -> [TemplateId]
listTemplateIds T.Stack{..} = stackTemplateId : mapMaybe extract stackParams
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
    
{-
listFilesRel :: MonadIO m => Path b Dir -> Stream (Of (Path Rel File)) m ()
listFilesRel dir = do
  (dirs, files) <- listDirRel dir
  S.each files <> foldMap (listFilesRel . (dir </>)) dirs
-}