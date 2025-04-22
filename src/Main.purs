module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)

import Game.Player (PlayerState, drawPlayer, getMovementComponent, handlePlayerMovement, withDefaultPaddleSize)
import Game.Utils (forced, intoRectangle)

import Graphics.Canvas (Context2D, clearRect, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

windowWidth :: Number
windowWidth = 1920.0

windowHeight :: Number
windowHeight = 1000.0

render :: Context2D -> PlayerState -> Effect Unit
render ctx st = do
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: windowWidth
    , height: windowHeight
    }

  setFillStyle ctx st.color

  { position
  , paddleWidth
  , paddleHeight
  , wasKeyPressed
  } <- getMovementComponent st

  let
    player = intoRectangle position paddleWidth paddleHeight

  when wasKeyPressed $ do
    drawPlayer ctx $ player

loop :: Context2D -> PlayerState -> Window -> Effect Unit
loop ctx st w =
  do
    setFillStyle ctx "#FFF"
    clearRect ctx { x: 0.0, y: 0.0, width: windowWidth, height: windowHeight }
    render ctx st
    void $ requestAnimationFrame (loop ctx st w) w

main :: Effect Unit
main = void $ forced do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  win <- window

  doc <- document win

  gameState <- withDefaultPaddleSize windowWidth windowHeight

  keyPressListener <- eventListener $ handlePlayerMovement gameState

  addEventListener keydown keyPressListener false (toEventTarget doc)

  loop ctx gameState win
