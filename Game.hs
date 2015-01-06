
module Game where

import Prelude

{-

  GameData stores all the state information per loop cycle, for example
  the paddle positions, ball position, score, etc.

-}

-- the amount of pixels to move while moving
paddleMovementSpeed :: Int
paddleMovementSpeed = 1

-- the amount of space between the paddles and the edges of the screen
paddleMarginBuffer :: Int
paddleMarginBuffer = 16

-- the states of movement for all objects (paddles and balls)
data ObjectState = Stationary | MovingWest | MovingEast | MovingNorth | MovingSouth | MovingNorthWest | MovingNorthEast | MovingSouthWest | MovingSouthEast

data GameData = GameData {
  -- left
    scoreLEFT         :: Int
  , paddleLEFTSTATE   :: ObjectState
  , paddleLEFTPOS     :: (Int, Int)
  -- right
  , scoreRIGHT        :: Int
  , paddleRIGHTSTATE  :: ObjectState
  , paddleRIGHTPOS    :: (Int, Int)
  -- ball
  , ballSTATE         :: ObjectState
  , ballPOS           :: (Int, Int)
}

newGameData :: Int -> Int -> GameData
newGameData w h = GameData {
  -- left
    scoreLEFT         = 0
  , paddleLEFTSTATE   = Stationary
  , paddleLEFTPOS     = ( paddleMarginBuffer, (h `div` 2) )
  -- right
  , scoreRIGHT        = 0
  , paddleRIGHTSTATE  = Stationary
  , paddleRIGHTPOS    = ( (w-paddleMarginBuffer), (h `div` 2) )
  -- ball
  , ballSTATE         = Stationary
  , ballPOS           = ( (w `div` 2), (h `div` 2) )
}
