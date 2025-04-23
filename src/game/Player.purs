module Game.Player
  ( PlayerState
  , newPlayer
  , withDefaultPaddleSize
  , handlePlayerMovement
  , drawPlayer
  ) where

import Prelude

import Data.Array (elem, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Game.Utils (Direction(..), Vec2, defaultPaddleSize, forced, intoRectangle, windowSize)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)
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

newPlayer :: Number -> Number -> Number -> Number -> Effect PlayerState
newPlayer windowWidth windowHeight paddleWidth paddleHeight = do
  playerWidth' <- Ref.new paddleWidth
  playerHeight' <- Ref.new paddleHeight
  position <- Ref.new
    { x: windowWidth / 2.0 - paddleWidth / 2.0
    , y: windowHeight / 2.0 + paddleHeight * 17.0
    }
  velocity <- Ref.new 65.0
  wasKeyPressed <- Ref.new false
  direction <- Ref.new Right
  pure $
    { color: "#D3D3D3"
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

keys :: Array String
keys = "ArrowLeft" : ("ArrowRight" : map ("Key" <> _) [ "A", "S", "D", "W" ])

changeDirection :: String -> Direction
changeDirection key =
  forced $
    case key of
      "KeyA" -> Left
      "ArrowLeft" -> Left
      "KeyD" -> Right
      "ArrowRight" -> Right

movePlayer :: Vec2 -> Number -> Direction -> Vec2
movePlayer { x, y } speed direction =
  let
    windowWidth = windowSize.width - defaultPaddleSize.width / 1.10
    windowStart = 0.0 - defaultPaddleSize.width / 3.10
  in
    case direction of
      Left -> { x: clamp windowStart windowWidth (x - speed), y }
      Right -> { x: clamp windowStart windowWidth (x + speed), y }

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

drawPlayer :: Context2D -> PlayerState -> Effect Unit
drawPlayer ctx { color, position, paddleWidth, paddleHeight } = do
  setFillStyle ctx color
  position' <- Ref.read position
  paddleWidth' <- Ref.read paddleWidth
  paddleHeight' <- Ref.read paddleHeight
  fillPath ctx $ rect ctx $ intoRectangle position' paddleWidth' paddleHeight'
