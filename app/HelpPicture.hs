{-# LANGUAGE TemplateHaskell #-}

module HelpPicture where

import Text

import Data.FileEmbed
import Data.Maybe
import System.FilePath (takeBaseName)
import Graphics.Gloss

renderHeader :: Maybe FilePath -> Picture
renderHeader filename = 
  pictures [brand, author, title, autosave]
  where
    brand = translate (-140) 300 $ scale 0.3 0.3 $ text "Jigsaw Sudoku"
    author = translate (-80) 270 $ renderText "made by Patrick Pang with <3"

    title = translate (-180) 200 $ scale 0.2 0.2 $ text $ maybe "" takeBaseName filename
    autosave = translate 100 200 $ renderText $ if isJust filename then "autosaved" else ""
  
usageString :: String
usageString = $(embedStringFile "assets/usage.txt")

commandsString :: String
commandsString = $(embedStringFile "assets/commands.txt")

rulesString :: String
rulesString = $(embedStringFile "assets/rules.txt")

renderUsage :: Picture
renderUsage = translate (-620) 200 $ renderParagraph usageString

renderCommands :: Picture
renderCommands = translate 260 200 $ renderParagraph commandsString

renderRules :: Picture
renderRules = translate (-180) (-280) $ renderParagraph rulesString
