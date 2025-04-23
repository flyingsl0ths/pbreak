module Game.Bricks where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Game.Utils (defaultPaddleSize)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)

type Color = String

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

toColor :: Int -> Color
toColor r =
  case r of
    0 -> "#FFB3B3"
    1 -> "#FFD1A3"
    2 -> "#FFFFB3"
    3 -> "#B3FFB3"
    4 -> "#B3FFFF"
    5 -> "#B3D1FF"
    6 -> "#C3B3FF"
    7 -> "#E3B3FF"
    _ -> "#D3D3D3"

drawBricks :: Context2D -> Effect Unit
drawBricks ctx = void $
  do
    forM_ 0 8 $ \r ->
      forM_ 0 19 $ \c ->
        do
          setFillStyle ctx $ toColor r
          fillPath ctx $ rect ctx
            { x: toNumber c * defaultPaddleSize.width + 11.0
            , y: toNumber r * defaultPaddleSize.height * 2.0
            , width: defaultPaddleSize.width - 5.0
            , height: defaultPaddleSize.height + 15.0
            }
