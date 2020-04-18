module LaunchPad.Version
  ( launchPadVersion
  , launchPadVersionString
  ) where

import Data.Version

import Paths_launchpad (version) 

import Relude.Custom

launchPadVersion :: Version
launchPadVersion = version

launchPadVersionString :: Text
launchPadVersionString = toText . showVersion $ version