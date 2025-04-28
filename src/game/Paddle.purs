module Game.Paddle
  ( PaddleState
  , newPaddle
  , withDefaultPaddleSize
  , handlePaddleMovement
  , drawPaddle
  , drawScore
  ) where

import Prelude

import Data.Array (elem, (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Game.Utils (Direction(..), Vec2, defaultPaddleSize, forced, intoRectangle, windowSize)
import Graphics.Canvas (Context2D, fillPath, fillText, rect, setFillStyle)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as KE

type PaddleState =
  { color :: String
  , position :: Ref Vec2
  , velocity :: Ref Number
  , wasKeyPressed :: Ref Boolean
  , direction :: Ref Direction
  , paddleWidth :: Ref Number
  , paddleHeight :: Ref Number
  , score :: Ref Int
  }

newPaddle :: Number -> Number -> Number -> Number -> Effect PaddleState
newPaddle windowWidth windowHeight paddleWidth paddleHeight = do
  position <- Ref.new
    { x: windowWidth / 2.0 - paddleWidth / 2.0
    , y: windowHeight / 2.0 + paddleHeight * 17.0
    }
  velocity <- Ref.new 65.0
  wasKeyPressed <- Ref.new false
  direction <- Ref.new Right
  playerWidth' <- Ref.new paddleWidth
  playerHeight' <- Ref.new paddleHeight
  score <- Ref.new 0
  pure $
    { color: "#D3D3D3"
    , position
    , velocity
    , wasKeyPressed
    , direction
    , paddleWidth: playerWidth'
    , paddleHeight: playerHeight'
    , score
    }

withDefaultPaddleSize :: Number -> Number -> Effect PaddleState
withDefaultPaddleSize windowWidth windowHeight =
  newPaddle
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

movePaddle :: Vec2 -> Number -> Direction -> Vec2
movePaddle { x, y } speed direction =
  let
    windowWidth = windowSize.width - defaultPaddleSize.width / 1.10
    windowStart = 0.0 - defaultPaddleSize.width / 3.10
  in
    case direction of
      Left -> { x: clamp windowStart windowWidth (x - speed), y }
      Right -> { x: clamp windowStart windowWidth (x + speed), y }

handlePaddleMovement
  :: PaddleState
  -> Event
  -> Effect Unit
handlePaddleMovement { position, velocity, wasKeyPressed, direction } e = do
  case KE.code <$> KE.fromEvent e of
    Just key' -> do
      let validKeyPressed = key' `elem` keys
      when validKeyPressed $ do
        Ref.modify' (const $ { state: validKeyPressed, value: unit }) wasKeyPressed
        direction' <- Ref.modify (const $ changeDirection key') direction
        v <- Ref.read velocity
        Ref.modify' (\pos -> { state: movePaddle pos v direction', value: unit })
          position
    Nothing -> pure unit

drawPaddle :: Context2D -> PaddleState -> Effect Unit
drawPaddle ctx { color, position, paddleWidth, paddleHeight } = do
  setFillStyle ctx color
  position' <- Ref.read position
  paddleWidth' <- Ref.read paddleWidth
  paddleHeight' <- Ref.read paddleHeight
  fillPath ctx $ rect ctx $ intoRectangle position' paddleWidth' paddleHeight'

drawScore :: Context2D -> PaddleState -> Effect Unit
drawScore ctx { score } = do
  score' <- Ref.read score
  let scoreText = "Score: " <> show score'
  setFillStyle ctx "#000"
  fillText
    ctx
    scoreText
    12.0
    30.0
