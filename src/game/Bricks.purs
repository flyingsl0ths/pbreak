module Game.Bricks
  ( Bricks
  , Brick
  , defaultBrickGridSize
  , genBricks
  , drawBricks
  ) where

import Prelude

import Game.Utils (defaultPaddleSize)

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)

type Color = String
type Brick = { x :: Number, y :: Number, color :: Ref String }

defaultBrickGridSize
  :: { columns :: Int
     , rows :: Int
     , width :: Number
     , height :: Number
     }
defaultBrickGridSize =
  { rows: 12
  , columns: 19
  , width: defaultPaddleSize.width - 5.0
  , height: defaultPaddleSize.height + 15.0
  }

type Bricks = Array (Effect Brick)

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
          { x: toNumber c * defaultPaddleSize.width + 11.0
          , y: toNumber r * defaultPaddleSize.height * 2.0 + 55.0
          , color
          }
    )
    (0 .. (rows * columns))

drawBricks :: Context2D -> Bricks -> Effect Unit
drawBricks ctx bs =
  for_ bs $ \b -> do
    { x, y, color } <- b
    color' <- Ref.read color
    setFillStyle ctx color'
    fillPath ctx
      $ rect ctx
          { x
          , y
          , width: defaultBrickGridSize.width
          , height: defaultBrickGridSize.height
          }
