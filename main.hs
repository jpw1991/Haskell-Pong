
{-# LANGUAGE FlexibleContexts #-} -- required for the state monad functions

import Prelude
import Data.Maybe

import Drawing
import Timer
import Game
import Input
import Logic

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
    
    -------------------------------
    -- Collision checking
    --
    -- We now need to check if the ball has met a paddle or a wall,
    -- in which case we need to invert the ball's direction.
    -- 
    -- If the ball meets the left wall or the right wall, we need
    -- to adjust the players' score.
    -------------------------------
    modifyGameData $ handleCollision b p1 p2
    
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
    
    -- show the score
    p1score <- liftIO $ SDLTTF.renderTextSolid font ("Red: " ++ (show $ scoreLEFT gdata)) (SDL.Color 255 10 10)
    p2score <- liftIO $ SDLTTF.renderTextSolid font ("Green: " ++ (show $ scoreRIGHT gdata)) (SDL.Color 10 255 10)
    applySurface'' 0 0 p1score screen Nothing
    applySurface'' (screenWidth `div` 2) 0 p2score screen Nothing
    
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
    
    invertBall :: GameData -> GameData
    invertBall old@GameData { ballSTATE = oldstate } =
      old { ballSTATE = (invertObjectState oldstate) }
    
    -- A function to invert the direction of an object.
    -- Used flip the ball's direction when it hits a wall
    -- or a paddle
    invertObjectState :: ObjectState -> ObjectState
    invertObjectState a
      | a == MovingWest      = MovingEast
      | a == MovingEast      = MovingWest
      | a == MovingNorth     = MovingSouth
      | a == MovingSouth     = MovingNorth
      | a == MovingNorthWest = MovingSouthEast
      | a == MovingNorthEast = MovingSouthWest
      | a == MovingSouthWest = MovingNorthEast
      | a == MovingSouthEast = MovingNorthWest
      | otherwise            = a
    
    -- Inverts an object state's direction but removes any diagonal
    -- movement.
    straightInvertObjectState :: ObjectState -> ObjectState
    straightInvertObjectState a
      | a == MovingWest      = MovingEast
      | a == MovingEast      = MovingWest
      | a == MovingNorth     = MovingNorth -- ?
      | a == MovingSouth     = MovingSouth -- ?
      | a == MovingNorthWest = MovingEast
      | a == MovingNorthEast = MovingWest
      | a == MovingSouthWest = MovingEast
      | a == MovingSouthEast = MovingWest
      | otherwise            = a
    
    -- Inverts an object state's direction and adds diagonal movement.
    diagonalInvertObjectState :: ObjectState -> ObjectState
    diagonalInvertObjectState a
      | a == MovingWest      = MovingNorthEast
      | a == MovingEast      = MovingNorthWest
      | a == MovingNorth     = MovingNorth -- ?
      | a == MovingSouth     = MovingSouth -- ?
      | a == MovingNorthWest = MovingSouthEast
      | a == MovingNorthEast = MovingSouthWest
      | a == MovingSouthWest = MovingNorthEast
      | a == MovingSouthEast = MovingNorthWest
      | otherwise            = a
    
    -- Another function used to invert the direction of an object.
    -- Instead of the direct opposite, the object is rotated slightly.
    halfInvertObjectState :: ObjectState -> ObjectState
    halfInvertObjectState a
      | a == MovingWest      = MovingNorth
      | a == MovingEast      = MovingSouth
      | a == MovingNorth     = MovingWest
      | a == MovingSouth     = MovingEast
      | a == MovingNorthWest = MovingSouthWest
      | a == MovingNorthEast = MovingSouthEast
      | a == MovingSouthWest = MovingNorthWest
      | a == MovingSouthEast = MovingNorthEast
    
    handleCollision :: SDL.Surface -> SDL.Surface -> SDL.Surface -> GameData -> GameData
    handleCollision ball p1 p2 old@GameData { ballSTATE = oldstate, scoreLEFT = oldleftscore, scoreRIGHT = oldrightscore, ballSPEED = oldspd, paddleLEFTSPEED = oldlspeed, paddleRIGHTSPEED = oldrspeed } =
      case (checkForCollision ball p1 p2 old) of
        LeftScores  -> old { ballSTATE = invertObjectState oldstate, ballPOS = ( (screenWidth `div` 2), (screenHeight `div` 2) ), scoreLEFT = (oldleftscore+1), ballSPEED = 2 }
        RightScores -> old { ballSTATE = invertObjectState oldstate, ballPOS = ( (screenWidth `div` 2), (screenHeight `div` 2) ), scoreRIGHT = (oldrightscore+1), ballSPEED = 2 }
        LeftPaddleCollision -> old { ballSTATE = straightInvertObjectState oldstate, ballSPEED = oldspd+1, paddleLEFTSPEED = oldlspeed+1, paddleRIGHTSPEED = oldrspeed+1 }
        RightPaddleCollision -> old { ballSTATE = straightInvertObjectState oldstate, ballSPEED = oldspd+1, paddleLEFTSPEED = oldlspeed+1, paddleRIGHTSPEED = oldrspeed+1 }
        LeftPaddleEdgeCollision -> old { ballSTATE = diagonalInvertObjectState oldstate, ballSPEED = oldspd+1, paddleLEFTSPEED = oldlspeed+1, paddleRIGHTSPEED = oldrspeed+1 }
        RightPaddleEdgeCollision -> old { ballSTATE = diagonalInvertObjectState oldstate, ballSPEED = oldspd+1, paddleLEFTSPEED = oldlspeed+1, paddleRIGHTSPEED = oldrspeed+1 }
        WallCollision -> old { ballSTATE = (halfInvertObjectState oldstate) }
        _ -> old
   
    
    checkForCollision :: SDL.Surface -> SDL.Surface -> SDL.Surface -> GameData -> CollisionResult
    checkForCollision ball p1 p2 gdata
      | (bx+bw) >= screenWidth  = LeftScores    -- hits right wall
      | bx <= 0                 = RightScores   -- hits left wall
      | (by+bh) >= screenHeight = WallCollision -- hits top
      | by <= 0                 = WallCollision -- hits bottom
      | boxesIntersect (bx,by) ((bx+bw),(by+bh)) (p1x,p1y) ((p1x + p1w),(p1y + p1h)) =
        if (by+bh) > (p1y + (p1h `div` 2) + (p1h `div` 4)) || (by+bh) < (p1y + (p1h `div` 2) - (p1h `div` 4)) then
          LeftPaddleEdgeCollision
        else
          LeftPaddleCollision
      | boxesIntersect (bx,by) ((bx+bw),(by+bh)) (p2x,p2y) ((p2x + p2w),(p2y + p2h)) =
        if (by+bh) > (p2y + (p2h `div` 2) + (p2h `div` 4)) || (by+bh) < (p2y + (p2h `div` 2) - (p2h `div` 4)) then
          RightPaddleEdgeCollision
        else
          RightPaddleCollision
      | otherwise               = NoCollision
      where
        bx = fst $ ballPOS gdata
        by = snd $ ballPOS gdata
        bw = SDL.surfaceGetWidth ball
        bh = SDL.surfaceGetHeight ball
        
        p1x = fst $ paddleLEFTPOS gdata
        p1y = snd $ paddleLEFTPOS gdata
        p1w = SDL.surfaceGetWidth p1
        p1h = SDL.surfaceGetHeight p1
        
        p2x = fst $ paddleRIGHTPOS gdata
        p2y = snd $ paddleRIGHTPOS gdata
        p2w = SDL.surfaceGetWidth p2
        p2h = SDL.surfaceGetHeight p2


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
--handleInput (SDL.KeyDown (SDL.Keysym SDL.SDLK_n _ _)) old = newGameData
handleInput _ old = old

handleKeyState :: KeyProc -> GameData -> GameData
handleKeyState key old
  | key SDL.SDLK_UP =
    old { paddleRIGHTSTATE = MovingNorth }
  | key SDL.SDLK_DOWN =
    old { paddleRIGHTSTATE = MovingSouth }
  | key SDL.SDLK_w =
    old { paddleLEFTSTATE = MovingNorth }
  | key SDL.SDLK_s =
    old { paddleLEFTSTATE = MovingSouth }
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
