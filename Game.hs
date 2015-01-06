
module Game where

import Prelude

import qualified Graphics.UI.SDL as SDL

{-

  GameData stores all the state information per loop cycle, for example
  the paddle positions, ball position, score, etc.

-}

-- the amount of pixels to move while moving
paddleMovementSpeed :: Int
paddleMovementSpeed = 2

-- the amount of space between the paddles and the edges of the screen
paddleMarginBuffer :: Int
paddleMarginBuffer = 16

-- the result after the ball collides with something
data CollisionResult = NoCollision | LeftScores | RightScores | LeftPaddleCollision | RightPaddleCollision | LeftPaddleEdgeCollision | RightPaddleEdgeCollision | WallCollision deriving (Eq, Show)

-- the states of movement for all objects (paddles and balls)
data ObjectState = Stationary | MovingWest | MovingEast | MovingNorth | MovingSouth | MovingNorthWest | MovingNorthEast | MovingSouthWest | MovingSouthEast deriving (Eq, Show)

data GameData = GameData {
  -- left
    scoreLEFT         :: Int
  , paddleLEFTSTATE   :: ObjectState
  , paddleLEFTPOS     :: (Int, Int)
  , paddleLEFTSPEED   :: Int
  -- right
  , scoreRIGHT        :: Int
  , paddleRIGHTSTATE  :: ObjectState
  , paddleRIGHTPOS    :: (Int, Int)
  , paddleRIGHTSPEED  :: Int
  -- ball
  , ballSTATE         :: ObjectState
  , ballPOS           :: (Int, Int)
  , ballSPEED         :: Int
}

newGameData :: Int -> Int -> SDL.Surface -> SDL.Surface -> SDL.Surface -> GameData
newGameData w h p1 p2 b = GameData {
  -- left
    scoreLEFT         = 0
  , paddleLEFTSTATE   = Stationary
  , paddleLEFTPOS     = ( paddleMarginBuffer, ((h `div` 2) - (p1h `div` 2)) )
  , paddleLEFTSPEED  = paddleMovementSpeed
  -- right
  , scoreRIGHT        = 0
  , paddleRIGHTSTATE  = Stationary
  , paddleRIGHTPOS    = ( ((w-paddleMarginBuffer) - p2w), ((h `div` 2) - (p1h `div` 2)) )
  , paddleRIGHTSPEED  = paddleMovementSpeed
  -- ball
  , ballSTATE         = MovingNorthEast
  , ballPOS           = ( ((w `div` 2) - (bw `div` 2)), ( (h `div` 2) - (bh `div` 2)) )
  , ballSPEED         = 2
}
  where
    p1w = SDL.surfaceGetWidth p1
    p1h = SDL.surfaceGetHeight p1
    p2w = SDL.surfaceGetWidth p2
    p2h = SDL.surfaceGetHeight p2
    bw  = SDL.surfaceGetWidth b
    bh  = SDL.surfaceGetHeight b

