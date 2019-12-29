{-# LANGUAGE TemplateHaskell #-}

module Text where

import Data.List.Split (splitOn)
import Data.FileEmbed
import Data.ByteString.Lazy (ByteString, fromStrict)
import Codec.Picture
import Graphics.Gloss
import Graphics.Text.TrueType
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Gloss.Juicy

fontData :: ByteString
fontData = fromStrict $(embedFile "assets/pangolin.ttf")

renderText :: String -> Picture
renderText s =
  case decodeFont fontData of
    Right font ->
      translate 240 10 $
      fromImageRGBA8 $
      renderDrawing 600 100 (PixelRGBA8 255 255 255 0) $
      withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
      printTextAt font (PointSize 12) (V2 60 60) s 
    Left _ -> scale 0.1 0.1 $ text s

renderParagraph :: String -> Picture
renderParagraph paragraph =
  case decodeFont fontData of
    Right font -> 
      translate 240 (-240) $
      fromImageRGBA8 $
      renderDrawing 600 600 (PixelRGBA8 255 255 255 0) $
      withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
      mapM_ (\(i, line) -> printTextAt font (PointSize 12) (V2 60 (60 + i * 20)) line) lines
    Left _ -> 
      pictures $ map (\(i, line) -> translate 0 (i * (-40)) $ scale 0.1 0.1 $ text line) lines
  where
    lines = zip [0..] $ splitOn "\n" paragraph