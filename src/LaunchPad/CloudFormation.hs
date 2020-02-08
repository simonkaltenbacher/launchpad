{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( AWSConstraint'
  , runCreateStack
  , runDeleteStack
  , runDeployStack
  , findStack
  )
  where

import           Control.Lens
import           Control.Lens.Setter               ((?~))
import           Control.Monad.Catch               (throwM)
import           Control.Monad.Reader

import           LaunchPad.CloudFormation.Internal
import           LaunchPad.Exception
import           LaunchPad.PrettyPrint
import           LaunchPad.Wait

import qualified Network.AWS.CloudFormation as CF

import           Path

import           Relude                            hiding (toText)


runCreateStack :: (AWSConstraint' m, PrettyPrint m) => Bool -> StackName -> Path Abs Dir -> m ()
runCreateStack disableRollback stackName resourceDir  = do
  (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
  uploadResources stack resourceDir
  withBlock ("Create stack " <> pretty _stackName) $ do
    createStack disableRollback stack
    await (stackCreateOrUpdateComplete "Creating stack") (createDescribeStackReq _stackName)
    reportSuccess "Stack creation complete"

runDeleteStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> m ()
runDeleteStack stackName = do
    (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
    withBlock ("Delete stack " <> pretty _stackName) $
      ifM (stackExists _stackName) deleteStack' err
  where
    deleteStack' = do
      deleteStack stackName
      await deleteStackComplete (createDescribeStackReq stackName)
      reportSuccess "Stack deletion complete"

    err = throwM . StackNotFoundException . renderDoc $ "Stack " <> pretty stackName <> " does not exist"

runDeployStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> Path Abs Dir -> m ()
runDeployStack stackName resourceDir = do
    (stack @ Stack{..})  <- findStack stackName =<< asks _stacks
    uploadResources stack resourceDir 
    withBlock ("Deploy stack " <> pretty _stackName) $ do
      resBucketName <- asks _resourceBucketName
      csType <- bool CF.Create CF.Update <$> stackExists _stackName
      csId <- createChangeSet csName csType stack
      csCreateComplete <- await changeSetCreateComplete (describeChangeSetReq csId)
      reportSuccess "Created change set"
      withBlock "The following changes will be applied:" $ printChanges csCreateComplete
      whenM (getConfirmation 3 parser "Do you want to continue? [y/n]: ") $ do
        executeChangeSet csId
        await (stackCreateOrUpdateComplete "Deploying stack") (createDescribeStackReq _stackName)
        reportSuccess "Stack deployment complete"
  where
    csName = ChangeSetName . unStackName $ stackName

    parser "y" = Just True
    parser "n" = Just False
    parser _   = Nothing

createDescribeStackReq :: StackName -> CF.DescribeStacks
createDescribeStackReq = flip (CF.dStackName ?~) CF.describeStacks . unStackName

describeChangeSetReq :: ChangeSetId -> CF.DescribeChangeSet
describeChangeSetReq csId = CF.describeChangeSet . unChangeSetId $ csId

uploadResources :: (AWSConstraint' m, PrettyPrint m) => Stack -> Path Abs Dir -> m ()
uploadResources stack resourceDir = withBlock "Uploading resources" $ do
    mapM_ uploadResource' (listResourceIds stack)
    reportSuccess "Upload complete"
  where
    uploadResource' rid = do
      putDocB $ pretty rid <> "... "
      uploadResource rid resourceDir 
      putTextLn "DONE"

printChanges :: (MonadIO m, PrettyPrint m) => CF.DescribeChangeSetResponse -> m ()
printChanges = mapM_ printChange . listChanges
  where
    listChanges
      = toListOf
      $ CF.desrsChanges
      . traverse
      . CF.cResourceChange
      . _Just

    printChange change = putTextBLn . fold . intersperse " " . fmap (fromMaybe "-") $
      [ fmap (renderDoc . pretty) . view CF.rcAction $ change
      , view CF.rcLogicalResourceId $ change
      , view CF.rcResourceType $ change
      ]

instance Pretty CF.ChangeAction where
  pretty CF.Add    = "ADD"
  pretty CF.Modify = "MODIFY"
  pretty CF.Remove = "REMOVE"
