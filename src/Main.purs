module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Game.Ball (BallState, drawBall, newBall, updateBall)
import Game.Bricks (Bricks, defaultBrickGridSize, drawBricks, genBricks)
import Game.Paddle (PaddleState, drawPaddle, drawScore, handlePaddleMovement, withDefaultPaddleSize)
import Game.Utils (forced, windowSize)
import Graphics.Canvas (Context2D, clearRect, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle, setFont)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type GameState =
  { paddle :: PaddleState
  , ball :: BallState
  , bricks :: Bricks
  }

render :: Context2D -> GameState -> Effect Unit
render ctx { paddle, ball, bricks } = do
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: windowSize.width
    , height: windowSize.height
    }

  drawScore ctx paddle

  drawBall ctx ball

  drawBricks ctx bricks

  drawPaddle ctx paddle

update :: GameState -> Effect Unit
update { ball } = updateBall ball

loop :: Context2D -> GameState -> Window -> Effect Unit
loop ctx st w =
  do
    setFillStyle ctx "#FFF"
    clearRect ctx { x: 0.0, y: 0.0, width: windowSize.width, height: windowSize.height }
    -- update st
    render ctx st
    void $ requestAnimationFrame (loop ctx st w) w

main :: Effect Unit
main = void $ forced do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  win <- window

  doc <- document win

  playerState <- withDefaultPaddleSize windowSize.width windowSize.height

  ballState <- newBall windowSize.width windowSize.height

  keyPressListener <- eventListener $ handlePaddleMovement playerState

  addEventListener keydown keyPressListener false (toEventTarget doc)

  setFont ctx "20px Joystix"

  let { rows, columns } = defaultBrickGridSize

  loop ctx { paddle: playerState, ball: ballState, bricks: genBricks rows columns } win
