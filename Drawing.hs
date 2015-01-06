
module Drawing where

import Data.Maybe
import Data.Word
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLImage

loadPNG :: String -> Maybe (Word8, Word8, Word8) -> IO SDL.Surface
loadPNG filename colourKey = SDLImage.load filename >>= SDL.displayFormat >>= setColorKey' colourKey

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src Nothing dst offset
  where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }

applySurface' :: Int -> Int -> SDL.Surface -> SDL.Surface -> Maybe SDL.Rect -> IO Bool
applySurface' x y src dst clip = SDL.blitSurface src clip dst offset
 where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }

--setColorKey' :: Maybe -> SDL.Surface -> SDL.Surface
setColorKey' Nothing s = return s
setColorKey' (Just (r,g,b)) surface = mapRGB' surface r g b >>= SDL.setColorKey surface [SDL.SrcColorKey] >> return surface

mapRGB' :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO SDL.Pixel
mapRGB' = SDL.mapRGB . SDL.surfaceGetPixelFormat
