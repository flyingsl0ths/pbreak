module Main where

import Prelude

import Data.Array (elem)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, clearRect, fillPath, getCanvasElementById, getContext2D, rect, setFillStyle, translate)
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (Window, window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

data Direction = Left | Right | Up | Down

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

type GameState =
  { playerSpeed :: Ref Number, wasKeyPressed :: Ref Boolean, direction :: Ref Direction }

type GameState' =
  { playerSpeed' :: Number, wasKeyPressed' :: Boolean, direction' :: Direction }

unpack :: GameState -> Effect GameState'
unpack { playerSpeed, wasKeyPressed, direction } =
  do
    playerSpeed' <- Ref.read playerSpeed
    wasKeyPressed' <- Ref.read wasKeyPressed
    direction' <- Ref.read direction
    pure $
      { playerSpeed', wasKeyPressed', direction' }

render :: Context2D -> GameState -> Effect Unit
render ctx st = void do
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 1280.0
    , height: 720.0
    }

  setFillStyle ctx "#0F0"

  st' <- unpack st

  when st'.wasKeyPressed' $ do
    case st'.direction' of
      Left -> translate ctx { translateX: -st'.playerSpeed', translateY: 0.0 }
      Right -> translate ctx { translateX: st'.playerSpeed', translateY: 0.0 }
      Up -> translate ctx { translateX: 0.0, translateY: -st'.playerSpeed' }
      Down -> translate ctx { translateX: 0.0, translateY: st'.playerSpeed' }

  fillPath ctx $ rect ctx
    { x: 20.0
    , y: 650.0
    , width: 200.0
    , height: 50.0
    }

loop :: Context2D -> GameState -> Window -> Effect Unit
loop ctx st w =
  do
    setFillStyle ctx "#FFF"
    clearRect ctx { x: 0.0, y: 0.0, width: 1280.0, height: 720.0 }
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
      "KeyW" -> Up
      "KeyS" -> Down

handleKeyPress
  :: GameState
  -> Event
  -> Effect Unit
handleKeyPress { wasKeyPressed, direction } e = do
  let key = maybe "" identity $ KE.code <$> KE.fromEvent e
  when (key /= "") $ do
    void $ Ref.modify (const $ key `elem` keys) wasKeyPressed
    void $ Ref.modify (const $ changeDirection key) direction

newGameState :: Effect GameState
newGameState = do
  playerSpeed <- Ref.new 10.0
  wasKeyPressed <- Ref.new false
  direction <- Ref.new Right
  pure $ { playerSpeed, wasKeyPressed, direction }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  win <- window

  doc <- document win

  gameState <- newGameState

  keyPressListener <- eventListener $ handleKeyPress gameState

  addEventListener keyup keyPressListener false (toEventTarget doc)

  loop ctx gameState win
