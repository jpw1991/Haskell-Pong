
module Input where

import Prelude
import Data.List (findIndices)

import Game

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as SDLUtil

import qualified System.IO.Unsafe as Unsafe

import Foreign
import Foreign.C.Types


-- `SDL_GetKeyState' is not defined in Graphic.UI.SDL
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

type KeyProc = SDL.SDLKey -> Bool

-- this function comes from mokehehe's super nario bros: http://github.com/mokehehe/monao in the file "AppUtil.hs"
getKeyState :: IO KeyProc
getKeyState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  if True
    then do -- for anarchy: Use unsafePerformIO
      let f = \k -> (/= 0) $ Unsafe.unsafePerformIO $ (peekByteOff keysPtr $ fromIntegral $ SDLUtil.fromEnum k :: IO Word8)
      return f
    else do -- for conservative
      numkeys <- peek numkeysPtr
      keys <- (map SDLUtil.toEnum . map fromIntegral . findIndices (== 1)) `fmap` peekArray (fromIntegral numkeys) keysPtr
      return $ (`elem` keys)
