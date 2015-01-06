
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

{- -- handleInput --

  Each button press will return a modified version of the game's data.
  For example, the user presses DOWN then the paddle's position needs
  to change; therefore a new version of the GameData is returned which
  contains this new position.

-}

-- With the @ operator we keep a reference to the old GameData copy
-- given to the function, and name its variables "oldx" and "oldy".
-- Then we return a fresh GameData with new values.
--
-- I don't permit the coordinates to go into the negative range, because
-- then we're scrolling the map off the screen.

handleInput :: SDL.Event -> GameData -> GameData
{-
handleInput (SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _))  old@GameData { player = oldplayer } =
  old {
    player = updatePlayerState MovingLeft oldplayer
  }
handleInput (SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _)) old@GameData { player = oldplayer } =
  old {
    player = updatePlayerState MovingRight oldplayer
  }
-}
handleInput _ old@GameData { } =
  old

handleKeyState :: KeyProc -> GameData -> GameData
handleKeyState key old@GameData { }
  | key SDL.SDLK_UP =
    old { paddleLEFTSTATE = MovingNorth }
  | key SDL.SDLK_DOWN =
    old { paddleLEFTSTATE = MovingSouth }
  | key SDL.SDLK_w =
    old { paddleRIGHTSTATE = MovingNorth }
  | key SDL.SDLK_s =
    old { paddleRIGHTSTATE = MovingSouth }
  | otherwise = old { paddleLEFTSTATE = Stationary, paddleRIGHTSTATE = Stationary }

notMoreThan :: Int -> Int -> Int
notMoreThan a b
  | a > b = b
  | otherwise = a

notLessThan :: Int -> Int -> Int
notLessThan a b
  | a < b = b
  | otherwise = a

notLessThanZero :: Int -> Int
notLessThanZero a
  | a < 0 = 0
  | otherwise = a

