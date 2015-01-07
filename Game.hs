
{-
    The MIT License
    Copyright (c) 2015 Joshua Woods

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
-}

module Game where

import Prelude

import qualified Graphics.UI.SDL as SDL

-- The default amount of pixels a paddle can move while moving.
paddleMovementSpeed :: Int
paddleMovementSpeed = 2

-- The amount of space between the paddles and the edges of the screen.
paddleMarginBuffer :: Int
paddleMarginBuffer = 16

-- The result after the ball collides with something.
data CollisionResult = NoCollision | LeftScores | RightScores | LeftPaddleCollision | RightPaddleCollision | LeftPaddleEdgeCollision | RightPaddleEdgeCollision | WallCollision deriving (Eq, Show)

-- The states of movement for all objects (paddles and balls).
data ObjectState = Stationary | MovingWest | MovingEast | MovingNorth | MovingSouth | MovingNorthWest | MovingNorthEast | MovingSouthWest | MovingSouthEast deriving (Eq, Show)

-- The GameData structure contains information about all the entities in the game.
data GameData = GameData {
  -- left paddle
    scoreLEFT         :: Int
  , paddleLEFTSTATE   :: ObjectState
  , paddleLEFTPOS     :: (Int, Int)
  , paddleLEFTSPEED   :: Int
  -- right paddle
  , scoreRIGHT        :: Int
  , paddleRIGHTSTATE  :: ObjectState
  , paddleRIGHTPOS    :: (Int, Int)
  , paddleRIGHTSPEED  :: Int
  -- ball
  , ballSTATE         :: ObjectState
  , ballPOS           :: (Int, Int)
  , ballSPEED         :: Int
}

-- Returns a fresh GameData structure.
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

