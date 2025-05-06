module Game.Ball where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Game.Paddle (defaultPaddleSize)
import Game.Utils (Vec2)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)

defaultBallSize :: Number
defaultBallSize = 10.0

defaultBallVelocity :: Vec2
defaultBallVelocity = { x: 3.0, y: -3.0 }

type BallState =
  { position :: Ref Vec2
  , velocity :: Ref Vec2
  }

newBall :: Number -> Number -> Effect BallState
newBall windowWidth windowHeight = do
  position <- Ref.new
    { x: windowWidth / 2.0 - defaultBallSize / 2.0
    , y: windowHeight / 2.0 + defaultPaddleSize.height * 16.5
    }
  velocity <- Ref.new defaultBallVelocity
  pure $ { position, velocity }

updateBall :: BallState -> Effect Unit
updateBall { position, velocity } = do
  vel <- Ref.read velocity
  position' <- Ref.modify'
    ( \pos ->
        let
          position' = { x: pos.x + vel.x, y: pos.y + vel.y }
        in
          { state: position', value: position' }
    )
    position
  logShow position'

drawBall :: Context2D -> BallState -> Effect Unit
drawBall ctx { position } = do
  pos <- Ref.read position
  setFillStyle ctx "#C0C0C0"
  fillPath ctx $ rect ctx
    { x: pos.x
    , y: pos.y
    , width: defaultBallSize
    , height: defaultBallSize
    }
