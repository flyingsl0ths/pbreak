module Main where

import Prelude

import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, Rectangle, clearRect, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle)
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

data Direction = Left | Right
type Vec2 = { x :: Number, y :: Number }

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

windowWidth :: Number
windowWidth = 1920.0

windowHeight :: Number
windowHeight = 1000.0

paddleWidth :: Number
paddleWidth = 200.0

paddleHeight :: Number
paddleHeight = 50.0

type GameState =
  { playerPosition :: Ref Vec2
  , playerSpeed :: Ref Number
  , wasKeyPressed :: Ref Boolean
  , direction :: Ref Direction
  }

type GameState' =
  { playerPosition' :: Vec2
  , playerSpeed' :: Number
  , wasKeyPressed' :: Boolean
  , direction' :: Direction
  }

readState :: GameState -> Effect GameState'
readState { playerPosition, playerSpeed, wasKeyPressed, direction } =
  do
    playerPosition' <- Ref.read playerPosition
    playerSpeed' <- Ref.read playerSpeed
    wasKeyPressed' <- Ref.read wasKeyPressed
    direction' <- Ref.read direction
    pure $
      { playerPosition', playerSpeed', wasKeyPressed', direction' }

intoRectangle :: Vec2 -> Number -> Number -> Rectangle
intoRectangle { x, y } width height = { x, y, width, height }

drawPlayer :: Context2D -> Rectangle -> Effect Unit
drawPlayer ctx r = fillPath ctx $ rect ctx r

render :: Context2D -> GameState -> Effect Unit
render ctx st = void do
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: windowWidth
    , height: windowHeight
    }

  setFillStyle ctx "#0F0"

  st' <- readState st

  let
    player = intoRectangle st'.playerPosition' paddleWidth paddleHeight

  when st'.wasKeyPressed' $ do
    case st'.direction' of
      Left -> drawPlayer ctx $ player
      Right -> drawPlayer ctx $ player

loop :: Context2D -> GameState -> Window -> Effect Unit
loop ctx st w =
  do
    setFillStyle ctx "#FFF"
    clearRect ctx { x: 0.0, y: 0.0, width: windowWidth, height: windowHeight }
    render ctx st
    void $ requestAnimationFrame (loop ctx st w) w

keys :: Array String
keys = map ("Key" <> _) [ "A", "S", "D", "W" ]

forced :: forall a. (Partial => a) -> a
forced = unsafePartial

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

handleKeyPress
  :: GameState
  -> Event
  -> Effect Unit
handleKeyPress { playerPosition, playerSpeed, wasKeyPressed, direction } e = do
  case KE.code <$> KE.fromEvent e of
    Just key' -> do
      let validKeyPressed = key' `elem` keys
      when validKeyPressed $ do
        Ref.modify' (const $ { state: validKeyPressed, value: unit }) wasKeyPressed
        direction' <- Ref.modify (const $ changeDirection key') direction
        v <- Ref.read playerSpeed
        Ref.modify' (\pos -> { state: movePlayer pos v direction', value: unit })
          playerPosition
    Nothing -> pure unit

newGameState :: Effect GameState
newGameState = do
  playerPosition <- Ref.new { x: 100.0, y: windowHeight / 2.0 + paddleHeight * 7.0 }
  playerSpeed <- Ref.new 50.0
  wasKeyPressed <- Ref.new false
  direction <- Ref.new Right
  pure $ { playerPosition, playerSpeed, wasKeyPressed, direction }

main :: Effect Unit
main = void $ forced do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  win <- window

  doc <- document win

  gameState <- newGameState

  keyPressListener <- eventListener $ handleKeyPress gameState

  addEventListener keydown keyPressListener false (toEventTarget doc)

  loop ctx gameState win
