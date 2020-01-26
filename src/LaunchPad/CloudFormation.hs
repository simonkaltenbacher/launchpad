{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( deployStack
  , findStack
  )
  where

import           Control.Lens
import           Control.Lens.Setter               ((?~))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS           hiding (await)

import           LaunchPad.CloudFormation.Internal
import           LaunchPad.PrettyPrint
import           LaunchPad.Wait

import qualified Network.AWS.CloudFormation as CF

import           Relude                            hiding (toText)


deployStack :: AWSConstraint' m => Bool -> Stack -> m ()
deployStack disableRollback (stack @ Stack{..}) = runPretty initPretty $ do
    withBlock "Uploading templates" $ mapM_ uploadResource' (listResourceIds stack)
    withBlock ("Deploying stack " <> pretty _stackName) $ do
      resBucketName <- asks _resourceBucketName
      csType <- either (const CF.Create) (const CF.Update)
        <$> trying _ServiceError (describeStack _stackName)
      csId <- createChangeSet csName csType stack
      csCreateComplete <- await changeSetCreateComplete (describeChangeSetReq csId)
      putDocBLn "Created change set successfully"
      withBlock "The following changes will be applied:" $ printChanges csCreateComplete
      whenM (getConfirmation 3 parser "Do you want to continue? [y/n]: ") $ do
        executeChangeSet csId
        await stackCreateOrUpdateComplete describeStackReq
        putDocBLn "Deployed stack successfully"
  where
    uploadResource' rid = do
      putDocB $ pretty rid <> "... "
      putTextLn "DONE"

    csName = ChangeSetName . unStackName $ _stackName

    describeChangeSetReq csId
      = CF.describeChangeSet
      . unChangeSetId $ csId

    describeStackReq
      = flip (CF.dStackName ?~) CF.describeStacks
      . unStackName $ _stackName

    parser "y" = Just True
    parser "n" = Just False
    parser _   = Nothing

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
