module Game.Ball where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Game.Utils (Vec2)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)

defaultBallSize :: Number
defaultBallSize = 10.0

defaultBallVelocity :: Vec2
defaultBallVelocity = { x: 10.0, y: -10.0 }

type BallState =
  { position :: Ref Vec2
  , velocity :: Ref Vec2
  }

newBall :: Number -> Number -> Effect BallState
newBall windowWidth windowHeight = do
  position <- Ref.new
    { x: windowWidth / 2.0 - defaultBallSize / 2.0
    , y: windowHeight / 2.0 + defaultBallSize
    }
  velocity <- Ref.new defaultBallVelocity
  pure $ { position, velocity }

updateBall :: BallState -> Effect Unit
updateBall { position, velocity } = do
  vel <- Ref.read velocity
  Ref.modify' (\pos -> { state: { x: pos.x + vel.x, y: pos.y + vel.y }, value: unit })
    position

drawBall :: Context2D -> BallState -> Effect Unit
drawBall ctx { position } = do
  pos <- Ref.read position
  setFillStyle ctx "#D3D3D3"
  fillPath ctx $ rect ctx
    { x: pos.x
    , y: pos.y
    , width: defaultBallSize
    , height: defaultBallSize
    }
