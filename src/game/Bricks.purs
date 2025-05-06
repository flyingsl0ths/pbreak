module Game.Bricks
  ( Bricks
  , Brick
  , defaultBrickGridSize
  , genBricks
  , drawBricks
  ) where

import Prelude

import Data.Array ((..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Game.Paddle (defaultPaddleSize)
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)

type Color = String
type Brick = { x :: Number, y :: Number, color :: Ref String }
type Bricks = Array (Effect Brick)

padding
  :: { x :: Number
     , y :: Number
     }
padding = { x: 5.0, y: 15.0 }

defaultBrickGridSize
  :: { columns :: Int
     , rows :: Int
     , width :: Number
     , height :: Number
     }
defaultBrickGridSize =
  { rows: 12
  , columns: 16
  , width: defaultPaddleSize.width - padding.x
  , height: defaultPaddleSize.height + padding.y
  }

brickGridOffset
  :: { x :: Number
     , y :: Number
     }
brickGridOffset = { x: 5.0, y: 60.0 }

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
    _ -> "#FFFFFF"

genBricks :: Int -> Int -> Bricks
genBricks rows columns =
  map
    ( \i -> do
        let r = i `div` columns
        let c = i `mod` columns
        color <- Ref.new (toColor r)
        pure $
          { x: toNumber c * defaultPaddleSize.width + brickGridOffset.x
          , y: toNumber r * defaultPaddleSize.height * 2.0 + brickGridOffset.y
          , color
          }
    )
    (0 .. (rows * columns))

drawBricks :: Context2D -> Bricks -> Effect Unit
drawBricks ctx bs =
  let
    totalColumns = defaultBrickGridSize.columns
    totalColumns' = totalColumns - 1
  in
    forWithIndex_ bs $ \i b -> do
      { x, y, color } <- b
      let c = i `mod` totalColumns
      color' <- Ref.read color
      setFillStyle ctx color'
      fillPath ctx
        $ rect ctx
            { x
            , y
            , width:
                if c == totalColumns' then
                  defaultBrickGridSize.width - padding.x
                else defaultBrickGridSize.width
            , height: defaultBrickGridSize.height
            }
