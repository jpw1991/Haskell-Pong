
{-# LANGUAGE FlexibleContexts #-} -- required for the state monad functions

import Prelude
import Data.Maybe

import Drawing
import Timer
import Game
import Input

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLImage
import qualified Graphics.UI.SDL.TTF as SDLTTF
import qualified Graphics.UI.SDL.TTF.General as SDLTTFG

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 600

windowCaption :: String
windowCaption = "Haskell Pong"

framesPerSecond :: Int
framesPerSecond = 60

---------------------------
-- Data
---------------------------

data AppData = AppData {
    fps      :: Timer
  , frame    :: Int
  , cap      :: Bool
  , font     :: SDLTTF.Font
  , gamedata :: GameData
}

data AppConfig = AppConfig {
    screen      :: SDL.Surface
  , background  :: SDL.Surface
  , paddle1     :: SDL.Surface
  , paddle2     :: SDL.Surface
  , ball        :: SDL.Surface
}

---------------------------
-- Types
---------------------------
type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

---------------------------
-- Functions
---------------------------

getFrame :: MonadState AppData m => m Int
getFrame = liftM frame get

getCap :: MonadState AppData m => m Bool
getCap = liftM cap get

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS timer = modify $ \s -> s { fps = timer }

putFrame :: MonadState AppData m => Int -> m ()
putFrame frm = modify $ \s -> s { frame = frm }

modifyFPS :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPS act = getFPS >>= act >>= putFPS

getFont :: MonadState AppData m => m SDLTTF.Font
getFont = liftM font get

getGameData :: MonadState AppData m => m GameData
getGameData = liftM gamedata get

modifyGameDataM :: MonadState AppData m => (GameData -> m GameData) -> m ()
modifyGameDataM act = getGameData >>= act >>= putGameData

modifyGameData :: MonadState AppData m => (GameData -> GameData) -> m ()
modifyGameData fn = fn `liftM` getGameData >>= putGameData

putGameData :: MonadState AppData m => GameData -> m()
putGameData gdata = modify $ \s -> s { gamedata = gdata }

{-- initEnv --
 Here we initiate the application's environment. Importantly, wire the correct
 actions for the key press messages. -}
initEnv :: IO (AppConfig, AppData)
initEnv = do
    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.SWSurface]
    SDL.setCaption windowCaption []
  
    background   <- loadPNG "background.png" Nothing
    font         <- SDLTTF.openFont "cour.ttf" 72
    p1           <- loadPNG "redpaddle.png" Nothing
    p2           <- loadPNG "greenpaddle.png" Nothing
    b            <- loadPNG "ball.png" (Just (0xff,0x00,0xff))
    
    
    ---- Game Data ----
    --gdata        <- return $ newGameData
    
    ---- Timer ----
    myTimer <- start defaultTimer
  
    ---- Finish ----
    -- Return the AppConfig and AppData together.
    return (AppConfig screen background p1 p2 b, AppData myTimer frame cap font (newGameData screenWidth screenHeight p1 p2 b))
    
  where
  
    cap = True
    frame = 0

{-- runLoop --
 runLoop starts the main loop with a reader to monitor the events. -}
runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

{-- loop --
 This is the program's main loop. It's responsible for drawing and handling
 user input. Each cycle of the loop takes a copy of the AppEnv, which itself
 contains a copy of the AppConfig and the AppData. -}
loop :: AppEnv ()
loop = do
    
    AppConfig screen background p1 p2 b <- ask
    
    ---- FPS ----
    modifyFPS $ liftIO . start
    
    ---- Handle the key presses ----
    quit <- whileEvents $ modifyGameData . handleInput
    
    ---- Handle the key states ----
    keystate <- liftIO getKeyState
    modifyGameData $ handleKeyState keystate
    
    -------------------------------
    -- The user has now changed the state of their paddles.
    -- We need to now modify the GameData depending upon this.
    --
    -- In other words, we now update the paddle and ball positions.
    -------------------------------
    
    
    modifyGameData $ handleLeftPaddleState
    modifyGameData $ handleRightPaddleState
    modifyGameData $ handleBallState
    
    --data1    <- getGameData
    --let p1 = player data1
    
    --modifyGameData $ updateGameData $ handleMState p1
    
    -------------------------------
    -- The GameData has now been updated.
    -- We can now move on to reflecting these changes upon the screen.
    -------------------------------
    
    -- lift the monad's layers away from the items
    -- stored within the state
    gdata <- getGameData
    font  <- getFont
    
    -- draw the background
    liftIO $ applySurface 0 0 background screen
    
    -- draw the paddles
    drawObject (paddleLEFTPOS gdata) p1 screen
    drawObject (paddleRIGHTPOS gdata) p2 screen
    
    -- draw the ball
    drawObject (ballPOS gdata) b screen
    
    
    ---- Update player pos ----
    -- The input has perhaps changed the velocity of the player, so we
    -- need to recalculate the player's position
    --modifyGameData $ updateGameData $ updatePlayerPos (player gdata)
    
    -- player should always be drawn in the center of the screen
    --let pldrawx = (div screenWidth 2)  - (div (SDL.surfaceGetWidth  $ playerlm !! 0) 2)
    --let pldrawy = (div screenHeight 2) - (div (SDL.surfaceGetHeight $ playerlm !! 0) 2)
    
    ---- Draw the scene ----
    liftIO $ SDL.flip screen
    
    ---- Delay ----
    -- if the FPS cap is enabled, delay if possible
    fps <- getFPS
    cap <- getCap
    
    liftIO $ do
      ticks <- getTimerTicks fps
      when (cap && ticks < secsPerFrame) $ do
        SDL.delay $ secsPerFrame - ticks
    
    unless quit loop
  
  where
    applySurface'' x y src dst clip = liftIO (applySurface' x y src dst clip)
    
    secsPerFrame = fromIntegral $ 1000 `div` framesPerSecond
    
    --drawPaddle :: (Int, Int) -> SDL.Surface -> SDL.Surface -> Bool
    drawObject pos src dst =
      liftIO $ applySurface x y src dst
      where
        x = fst $ pos
        y = snd $ pos
    
    handleLeftPaddleState :: GameData -> GameData
    handleLeftPaddleState old@GameData { paddleLEFTSTATE = state, paddleLEFTPOS = pos }
      | state == MovingNorth = old { paddleLEFTPOS = ( x, y-paddleMovementSpeed ) }
      | state == MovingSouth = old { paddleLEFTPOS = ( x, y+paddleMovementSpeed ) }
      | otherwise            = old
      where
        x = fst $ pos
        y = snd $ pos
    
    handleRightPaddleState :: GameData -> GameData
    handleRightPaddleState old@GameData { paddleRIGHTSTATE = state, paddleRIGHTPOS = pos }
      | state == MovingNorth = old { paddleRIGHTPOS = ( x, y-paddleMovementSpeed ) }
      | state == MovingSouth = old { paddleRIGHTPOS = ( x, y+paddleMovementSpeed ) }
      | otherwise            = old
      where
        x = fst $ pos
        y = snd $ pos
    
    handleBallState :: GameData -> GameData
    handleBallState old@GameData { ballSTATE = state, ballPOS = pos, ballSPEED = spd }
      | state == MovingWest      = old { ballPOS = ( x - spd, y ) }
      | state == MovingEast      = old { ballPOS = ( x + spd, y ) }
      | state == MovingNorth     = old { ballPOS = ( x, y - spd ) }
      | state == MovingSouth     = old { ballPOS = ( x, y + spd ) }
      | state == MovingNorthWest = old { ballPOS = ( x - spd, y - spd ) }
      | state == MovingNorthEast = old { ballPOS = ( x + spd, y - spd ) }
      | state == MovingSouthWest = old { ballPOS = ( x - spd, y + spd ) }
      | state == MovingSouthEast = old { ballPOS = ( x + spd, y + spd ) }
      | otherwise                = old
      where
        x = fst $ pos
        y = snd $ pos


{-- whileEvents --
 This helps process the events. -}
whileEvents :: MonadIO m => (SDL.Event -> m()) -> m Bool
whileEvents act = do
  event <- liftIO SDL.pollEvent
  case event of
    SDL.Quit -> return True
    SDL.NoEvent -> return False
    _ -> do
      act event
      whileEvents act

---------------------------
-- Input
---------------------------
{- -- handleInput --

  Each button press will return a modified version of the game's data.
  For example, the user presses DOWN then the paddle's position needs
  to change; therefore a new version of the GameData is returned which
  contains this new position.

-}

-- With the @ operator we keep a reference to the old GameData copy
-- given to the function, and name its variables "oldx" and "oldy".
-- Then we return a fresh GameData with new values.

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


---------------------------
-- Main
---------------------------

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  
  result <- SDLTTFG.init
  if not result
    then putStr "Failed to init ttf\n"
  else
    do
      (env, state) <- initEnv
      
      runLoop env state
      
      SDLTTFG.quit
