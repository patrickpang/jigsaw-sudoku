{-# LANGUAGE TemplateHaskell #-}

module Help where

import Data.FileEmbed
import Data.String

usageString :: String
usageString = $(embedStringFile "assets/usage.txt")

commandsString :: String
commandsString = $(embedStringFile "assets/commands.txt")