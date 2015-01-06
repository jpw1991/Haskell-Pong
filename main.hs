
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
screenWidth = 1024

screenHeight :: Int
screenHeight = 768

windowCaption :: String
windowCaption = "Haskell Pong"

framesPerSecond :: Int
framesPerSecond = 60

---------------------------
-- Data
---------------------------

data AppData = AppData {
    fps :: Timer
  , frame :: Int
  , cap :: Bool
  , font :: SDLTTF.Font
  , gamedata :: GameData
}

data AppConfig = AppConfig {
    screen    :: SDL.Surface
  , background :: SDL.Surface
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
    
    ---- Game Data ----
    --gdata        <- return $ newGameData
    
    ---- Timer ----
    myTimer <- start defaultTimer
  
    ---- Finish ----
    -- Return the AppConfig and AppData together.
    return (AppConfig screen background, AppData myTimer frame cap font gdata)
    
  where
  
    cap = True
    frame = 0
    gdata = newGameData screenWidth screenHeight

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
    
    AppConfig screen background <- ask
    
    ---- FPS ----
    modifyFPS $ liftIO . start
    
    ---- Handle the key presses ----
    quit <- whileEvents $ modifyGameData . handleInput
    
    ---- Handle the key states ----
    keystate <- liftIO getKeyState
    modifyGameData $ handleKeyState keystate
    
    -------------------------------
    -- The user has now changed the state of their submarine.
    -- We need to now update the player values depending upon this.
    -------------------------------
    --data1    <- getGameData
    --let p1 = player data1
    
    --modifyGameData $ updateGameData $ handleMState p1
    
    -------------------------------
    -- The player's position, state, etc have now all been updated.
    -- We can now move on to reflecting these changes upon the screen.
    -------------------------------
    
    -- lift the monad's layers away from the items
    -- stored within the state
    --hexmap   <- getHexmap
    --tileset  <- getTileset
    --font     <- getFont
    --gdata    <- getGameData
    --playerlm <- getPlayerLM
    --playerrm <- getPlayerRM
    --playerla <- getPlayerLA
    --playerra <- getPlayerRA
    
    ---- Update player pos ----
    -- The input has perhaps changed the velocity of the player, so we
    -- need to recalculate the player's position
    --modifyGameData $ updateGameData $ updatePlayerPos (player gdata)
    
    -- draw the background
    liftIO $ applySurface 0 0 background screen
    
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
