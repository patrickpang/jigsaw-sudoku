module TextPicture where

import Help

import Data.List.Split (splitOn)
import Data.Maybe
import System.FilePath (takeBaseName)
import Graphics.Gloss

renderHeader :: Maybe FilePath -> Picture
renderHeader filename = 
  pictures [brand, author, title, autosave]
  where
    brand = translate (-140) 300 $ scale 0.3 0.3 $ text "Jigsaw Sudoku"
    author = translate (-80) 270 $ scale 0.1 0.1 $ text "made by Patrick Pang"

    title = translate (-180) 200 $ scale 0.2 0.2 $ text $ maybe "" takeBaseName filename
    autosave = translate 120 200 $ scale 0.1 0.1 $ text $ if isJust filename then "autosaved" else ""
  
renderUsage :: Picture
renderUsage = translate (-600) 200 $ renderText usageString

renderCommands :: Picture
renderCommands = translate 260 200 $ renderText commandsString

renderText :: String -> Picture
renderText s = pictures
  [
    translate 0 (i * (-height)) $ scale 0.1 0.1 $ text line
    | (i, line) <- zip [0..] $ splitOn "\n" s
  ]
  where height = 20