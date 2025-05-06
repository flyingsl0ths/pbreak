module Game.Utils
  ( Direction(..)
  , Vec2
  , intoRectangle
  , forced
  , windowSize
  , forM_
  ) where

import Prelude
import Graphics.Canvas (Rectangle)
import Partial.Unsafe (unsafePartial)

data Direction = Left | Right
type Vec2 = { x :: Number, y :: Number }

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

intoRectangle :: Vec2 -> Number -> Number -> Rectangle
intoRectangle { x, y } width height = { x, y, width, height }

forced :: forall a. (Partial => a) -> a
forced = unsafePartial

windowSize :: { width :: Number, height :: Number }
windowSize = { width: 1920.0, height: 1000.0 }

forM_ :: forall m a. Monad m => Int -> Int -> (Int -> m a) -> m Unit
forM_ lo hi f
  | lo < hi = iter 0
      where
      iter i =
        if i == hi then (pure unit)
        else
          ( do
              void $ f i
              iter (i + 1)
          )
  | otherwise = pure unit
