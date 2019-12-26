module Colors where

import Graphics.Gloss.Data.Color

colorsOfBlocks :: [Color]
colorsOfBlocks = map (withAlpha 0.5)
  [
    red,
    green,
    blue,
    yellow,
    cyan,
    magenta,
    rose,
    chartreuse,
    azure
  ]