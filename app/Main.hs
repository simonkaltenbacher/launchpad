{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Conduit

import Control.Exception        (IOException)
import Control.Exception.Lens   (handling)
import Control.Monad.Catch      (handle, MonadCatch)
import Control.Monad.Reader
import Control.Monad.Trans.AWS

import LaunchPad.CloudFormation
import LaunchPad.Config
import LaunchPad.Exception
import LaunchPad.PrettyPrint
import LaunchPad.Version

import Options.Applicative

import Path.IO

import Relude


data Options = Options
  { confFile  :: FilePath
  , mode      :: Mode
  }
  
data Mode
  = Create
      { disableRollback :: Bool
      , stackName       :: StackName
      , resourceDir     :: FilePath
      }
  | Delete
      { stackName :: StackName
      }
  | Deploy
      { stackName   :: StackName
      , resourceDir :: FilePath
      }

main :: IO ()
main = run =<< execParser (info parser infoMods)
  where
    infoMods = fullDesc <> header
      (  "launchpad "
      <> launchPadVersionString
      <> " - Simplify deployment of nested stacks"
      )

    parser = Options
      <$>  confFileOpt
      <*>  subparser (createCmd <> deleteCmd <> deployCmd)
      <**> helper

    run Options{..} = handleError $ do
      conf <- readConfig =<< resolveFile' confFile
      runResourceT . runAWST conf . runPretty initPretty $ do
        case mode of
          Create{..} -> runCreateStack disableRollback stackName =<< resolveDir' resourceDir
          Delete{..} -> runDeleteStack stackName
          Deploy{..} -> runDeployStack stackName =<< resolveDir' resourceDir

createCmd :: Mod CommandFields Mode
createCmd = command "create" $ info parser infoMods
  where
    parser = Create
      <$>  disableRollbackSwitch
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Create given stack" <> createHeader

    createHeader = header $ "Create given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

deleteCmd :: Mod CommandFields Mode
deleteCmd = command "delete" $ info parser infoMods
  where
    parser = Delete
      <$>  stackNameArg
      <**> helper

    infoMods = progDesc "Delete given stack" <> createHeader

    createHeader = header $ "Delete given stack with name STACK_NAME as specified in CONF_FILE."

deployCmd :: Mod CommandFields Mode
deployCmd = command "deploy" $ info parser infoMods
  where
    parser = Deploy
      <$>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Deploy given stack" <> deployHeader

    deployHeader = header $ "Deploy given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

confFileOpt :: Parser FilePath
confFileOpt = strOption $
     short   'c'
  <> long    "conf"
  <> metavar "CONF_FILE"
  <> help    "Dhall configuration file containing the deployment configuration for each stack"

disableRollbackSwitch :: Parser Bool
disableRollbackSwitch = switch $
     long "disable-rollback"
  <> help "Disable rollback of stack if stack creation fails"

stackNameArg :: Parser StackName
stackNameArg = argument auto $
     metavar "STACK_NAME"
  <> help    "Name of the stack"

resourceDirArg :: Parser FilePath
resourceDirArg = strArgument $
     metavar "RESOURCE_DIR"
  <> help    "Directory where resources such as templates and scripts are located"  

handleError :: forall m. (MonadCatch m, MonadIO m) => m () -> m ()
handleError
    = handle reportIOException
    . handle reportAWSError
    . handling _ServiceError reportServiceError
    . handle reportLaunchPadException
  where
    reportServiceError = reportError . pretty

    reportIOException :: IOException -> m ()
    reportIOException = reportError . pretty

    reportAWSError :: Error -> m ()
    reportAWSError = reportError . pretty

    reportLaunchPadException :: SomeLaunchPadException -> m ()
    reportLaunchPadException = reportError . pretty