module Game.Utils
  ( Direction(..)
  , Vec2
  , intoRectangle
  , forced
  , defaultPaddleSize
  ) where

import Graphics.Canvas (Rectangle)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord)

data Direction = Left | Right
type Vec2 = { x :: Number, y :: Number }

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

intoRectangle :: Vec2 -> Number -> Number -> Rectangle
intoRectangle { x, y } width height = { x, y, width, height }

forced :: forall a. (Partial => a) -> a
forced = unsafePartial

defaultPaddleSize :: { width :: Number, height :: Number }
defaultPaddleSize = { width: 100.0, height: 20.0 }
