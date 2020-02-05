module LaunchPad.Version
  ( launchPadVersion
  , launchPadVersionString
  ) where

import Data.String               (String)
import Data.Version

import Paths_launchpad           (version) 


launchPadVersion :: Version
launchPadVersion = version

launchPadVersionString :: String
launchPadVersionString = showVersion version