{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Conduit

import           Control.Exception        (handle, IOException)
import           Control.Exception.Lens   (handling)
import           Control.Lens.Getter      (view)
import           Control.Monad            ((=<<), join)
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS

import           Data.Text                (pack)

import           LaunchPad.CloudFormation
import           LaunchPad.Config

import           Options.Applicative

import           Path.IO

import           Relude



main :: IO ()
main = reportError . join . execParser $ info parser infoMods
  where
    infoMods = fullDesc <> header "launchpad 0.1.1 - Automate deployment of nested stacks"

parser :: Parser (IO ())
parser = subparser deployCmd <**> helper

deployCmd :: Mod CommandFields (IO ())
deployCmd = command "deploy" $ info parser infoMods
  where
    parser = run
      <$>  confFileOpt
      <*>  disableRollbackSwitch
      <*>  stackNameArg
      <*>  templateDirArg
      <**> helper

    infoMods = progDesc $ "Deploy given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory TEMPLATE_DIR."

    run confFile disableRollback stackName templateDir = do
      conf <- join $ readConfig <$> resolveDir' templateDir <*> resolveFile' confFile
      runResourceT . runAWST conf $ do
        stack <- findStack stackName =<< asks _stacks
        stackId <- deployStack disableRollback stack
        liftIO $ putTextLn $ "Tracking status of stack " <> unStackName stackName
        trackStackStatus stackName

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

templateDirArg :: Parser FilePath
templateDirArg = strArgument $
     metavar "TEMPLATE_DIR"
  <> help    "Directory where template files are located"  

reportError :: IO () -> IO ()
reportError
    = handle reportIOError
    . handle reportAWSError
    . handling _ServiceError reportServiceError
  where
    reportServiceError error = putTextLn $
           "ERROR "
        <> "ServiceError "
        <> extractErrorCode error
        <> ": "
        <> extractErrorMessage error
      where
        extractErrorCode = (\(ErrorCode c) -> c) . view serviceCode
        extractErrorMessage = foldMap (\(ErrorMessage m) -> m) . view serviceMessage

    reportIOError :: IOException -> IO ()
    reportIOError = reportError

    reportAWSError :: Error -> IO ()
    reportAWSError = reportError

    reportError :: Show e => e -> IO ()
    reportError = putTextLn . ("ERROR " <>) . pack . show