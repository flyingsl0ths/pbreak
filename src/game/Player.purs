module Game.Player
  ( PlayerState
  , MovementComponent
  , newPlayer
  , withDefaultPaddleSize
  , getMovementComponent
  , handlePlayerMovement
  , drawPlayer
  ) where

import Prelude

import Game.Utils (Direction(..), Vec2, defaultPaddleSize, forced)

import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, Rectangle, fillPath, rect)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as KE

type PlayerState =
  { color :: String
  , position :: Ref Vec2
  , velocity :: Ref Number
  , wasKeyPressed :: Ref Boolean
  , direction :: Ref Direction
  , paddleWidth :: Ref Number
  , paddleHeight :: Ref Number
  }

type MovementComponent =
  { position :: Vec2
  , paddleWidth :: Number
  , paddleHeight :: Number
  , wasKeyPressed :: Boolean
  , direction :: Direction
  }

newPlayer :: Number -> Number -> Number -> Number -> Effect PlayerState
newPlayer windowWidth windowHeight paddleWidth paddleHeight = do
  playerWidth' <- Ref.new paddleWidth
  playerHeight' <- Ref.new paddleHeight
  position <- Ref.new
    { x: windowWidth / 2.0 - paddleWidth, y: windowHeight / 2.0 + paddleHeight * 7.0 }
  velocity <- Ref.new 50.0
  wasKeyPressed <- Ref.new false
  direction <- Ref.new Right
  pure $
    { color: "#0F0"
    , position
    , velocity
    , wasKeyPressed
    , direction
    , paddleWidth: playerWidth'
    , paddleHeight: playerHeight'
    }

withDefaultPaddleSize :: Number -> Number -> Effect PlayerState
withDefaultPaddleSize windowWidth windowHeight =
  newPlayer
    windowWidth
    windowHeight
    defaultPaddleSize.width
    defaultPaddleSize.height

getMovementComponent :: PlayerState -> Effect MovementComponent
getMovementComponent
  { position, paddleWidth, paddleHeight, wasKeyPressed, direction } = do
  position' <- Ref.read position
  paddleWidth' <- Ref.read paddleWidth
  paddleHeight' <- Ref.read paddleHeight
  wasKeyPressed' <- Ref.read wasKeyPressed
  direction' <- Ref.read direction
  pure
    { position: position'
    , paddleWidth: paddleWidth'
    , paddleHeight: paddleHeight'
    , wasKeyPressed: wasKeyPressed'
    , direction: direction'
    }

keys :: Array String
keys = map ("Key" <> _) [ "A", "S", "D", "W", "ArrowLeft", "ArrowRight" ]

changeDirection :: String -> Direction
changeDirection key =
  forced $
    case key of
      "KeyA" -> Left
      "KeyD" -> Right

movePlayer :: Vec2 -> Number -> Direction -> Vec2
movePlayer { x, y } speed direction =
  case direction of
    Left -> { x: x - speed, y }
    Right -> { x: x + speed, y }

handlePlayerMovement
  :: PlayerState
  -> Event
  -> Effect Unit
handlePlayerMovement { position, velocity, wasKeyPressed, direction } e = do
  case KE.code <$> KE.fromEvent e of
    Just key' -> do
      let validKeyPressed = key' `elem` keys
      when validKeyPressed $ do
        Ref.modify' (const $ { state: validKeyPressed, value: unit }) wasKeyPressed
        direction' <- Ref.modify (const $ changeDirection key') direction
        v <- Ref.read velocity
        Ref.modify' (\pos -> { state: movePlayer pos v direction', value: unit })
          position
    Nothing -> pure unit

drawPlayer :: Context2D -> Rectangle -> Effect Unit
drawPlayer ctx r = fillPath ctx $ rect ctx r
