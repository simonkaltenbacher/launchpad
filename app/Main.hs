{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Conduit

import Control.Exception        (IOException)
import Control.Exception.Lens   (handling)
import Control.Monad            ((=<<), join)
import Control.Monad.Catch      (handle, MonadCatch)
import Control.Monad.Reader
import Control.Monad.Trans.AWS

import LaunchPad.CloudFormation
import LaunchPad.Config
import LaunchPad.Exception
import LaunchPad.PrettyPrint

import Options.Applicative

import Path.IO

import Relude


main :: IO ()
main = join . liftIO . execParser $ info parser infoMods
  where
    infoMods = fullDesc <> header "launchpad 0.2.0 - Simplify deployment of nested stacks"

parser :: Parser (IO ())
parser = subparser commands <**> helper
  where
    commands = createCmd <> deleteCmd <> deployCmd

createCmd :: Mod CommandFields (IO ())
createCmd = command "create" $ info parser infoMods
  where
    parser = run
      <$>  confFileOpt
      <*>  disableRollbackSwitch
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Create given stack" <> createHeader

    createHeader = header $ "Create given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

    run confFile disableRollback stackName resourceDir = handleError $ do
      conf <- join $ readConfig <$> resolveDir' resourceDir <*> resolveFile' confFile
      runResourceT . runAWST conf . runPretty initPretty $ do
        stack <- findStack stackName =<< asks _stacks
        void $ createStackAction disableRollback stack

deleteCmd :: Mod CommandFields (IO ())
deleteCmd = command "delete" $ info parser infoMods
  where
    parser = run
      <$>  confFileOpt
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Delete given stack" <> createHeader

    createHeader = header $ "Delete given stack with name STACK_NAME as specified in CONF_FILE."

    run confFile stackName resourceDir = handleError $ do
      conf <- join $ readConfig <$> resolveDir' resourceDir <*> resolveFile' confFile
      runResourceT . runAWST conf . runPretty initPretty $ do
        stack <- findStack stackName =<< asks _stacks
        void $ deleteStackAction stack

deployCmd :: Mod CommandFields (IO ())
deployCmd = command "deploy" $ info parser infoMods
  where
    parser = run
      <$>  confFileOpt
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Deploy given stack" <> deployHeader

    deployHeader = header $ "Deploy given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

    run confFile stackName resourceDir = handleError $ do
      conf <- join $ readConfig <$> resolveDir' resourceDir <*> resolveFile' confFile
      runResourceT . runAWST conf . runPretty initPretty $ do
        stack <- findStack stackName =<< asks _stacks
        void $ deployStackAction stack

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
  <> help    "Name of the stack to be deployed"

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