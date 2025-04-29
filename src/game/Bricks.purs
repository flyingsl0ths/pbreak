module Game.Bricks where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Game.Utils (defaultPaddleSize, forM_)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)

type Color = String

toColor :: Int -> Color
toColor r =
  case r of
    0 -> "#FFB3B3"
    1 -> "#AFCBFF"
    2 -> "#FFE5B4"
    3 -> "#FFD1A3"
    4 -> "#FFFFB3"
    5 -> "#B3FFB3"
    6 -> "#B3FFFF"
    7 -> "#B3D1FF"
    8 -> "#C3B3FF"
    9 -> "#E3B3FF"
    10 -> "#F4C2C2"
    11 -> "#BFD8B8"
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
            , y: toNumber r * defaultPaddleSize.height * 2.0 + 55.0
            , width: defaultPaddleSize.width - 5.0
            , height: defaultPaddleSize.height + 15.0
            }
