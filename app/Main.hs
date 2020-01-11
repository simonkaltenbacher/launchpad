{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Lens.Setter        ((.~))
import           Control.Monad              ((=<<), join)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Reader

import           Data.Bifunctor             (first)
import           Data.Functor               ((<&>))
import           Data.Text                  (Text)
import           Data.Text.IO               (putStrLn)

import           Dhall

import           LaunchPad.Deploy
import           LaunchPad.Type             (Config)
import qualified LaunchPad.Type as T
import           LaunchPad.Type.Dhall       (DhallConfig)
import qualified LaunchPad.Type.Dhall as TD

import           Options.Applicative

import           Path

import           Prelude                    hiding (putStrLn)

import           TextShow                   (showt)


main :: IO ()
main = join $ execParser opts

opts :: ParserInfo (IO ())
opts = info (run <$> confFileOpt <*> stackNameArg <*> templateDirArg <**> helper) idm
  where
    run confFile stackName templateDir = do
      conf <- readConfig templateDir confFile
      stackId <- (runResourceT . runAWST conf . deployStack) stackName
      putStrLn $ "Stack " <> showt stackId

confFileOpt :: Parser (Path Abs File)
confFileOpt = option (eitherReader $ first show . parseAbsFile) $
     short 'c'
  <> long "conf"
  <> metavar "CONF_FILE"

stackNameArg :: Parser Text
stackNameArg = strArgument (metavar "STACK_NAME")

templateDirArg :: Parser (Path Abs Dir)
templateDirArg = argument (eitherReader $ first show . parseAbsDir) (metavar "TEMPLATE_DIR")

readConfig :: Path Abs Dir -> Path Abs File -> IO Config
readConfig templateDir confFile = do
  env <- newEnv Discover <&> envRegion .~ Frankfurt
  TD.DhallConfig {..} <- readDhallConfig confFile
  return T.Config {..}

readDhallConfig :: Path Abs File -> IO DhallConfig
readDhallConfig = inputFile (autoWith interpretOptions) . toFilePath
  where
    interpretOptions = defaultInterpretOptions { singletonConstructors = Bare }