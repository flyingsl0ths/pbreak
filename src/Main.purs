module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import Game.Bricks (drawBricks)
import Game.Paddle (PaddleState, drawPaddle, drawScore, handlePaddleMovement, withDefaultPaddleSize)
import Game.Utils (forced, windowSize)

import Graphics.Canvas (Context2D, clearRect, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

render :: Context2D -> PlayerState -> Effect Unit
render ctx st = do
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: windowSize.width
    , height: windowSize.height
    }

  drawBricks ctx

  drawPlayer ctx st

loop :: Context2D -> PlayerState -> Window -> Effect Unit
loop ctx st w =
  do
    setFillStyle ctx "#FFF"
    clearRect ctx { x: 0.0, y: 0.0, width: windowSize.width, height: windowSize.height }
    render ctx st
    void $ requestAnimationFrame (loop ctx st w) w

main :: Effect Unit
main = void $ forced do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  win <- window

  doc <- document win

  playerState <- withDefaultPaddleSize windowSize.width windowSize.height

  keyPressListener <- eventListener $ handlePlayerMovement gameState

  addEventListener keydown keyPressListener false (toEventTarget doc)

  loop ctx gameState win
