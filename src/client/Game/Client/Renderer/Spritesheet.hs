module Game.Client.Renderer.Spritesheet (
  Spritesheet,
  spritesheetTexture,
  spritesheetCellSize,
  spritesheetGridSize,
  spritesheetCells
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Aeson hiding (Array)

import Linear

data Spritesheet = Spritesheet
  { spritesheetTexture :: Text
  , spritesheetCellSize :: V2 Int
  , spritesheetGridSize :: V2 Int
  , spritesheetCells :: Vector (V2 Int)
  } deriving (Show)

newSpritesheet :: Text -> V2 Int -> V2 Int -> Spritesheet
newSpritesheet texture cellSize@(V2 cw ch) gridSize@(V2 gw gh) =
  let
    xs = Vector.enumFromN 0 gw
    ys = Vector.enumFromN 0 gh
    cells = Vector.concatMap (\y -> Vector.map (\x -> V2 (x * cw) (y * ch)) ys) xs
  in
    Spritesheet {
        spritesheetTexture = texture
      , spritesheetCellSize = cellSize
      , spritesheetGridSize = gridSize
      , spritesheetCells = cells
      }

instance FromJSON Spritesheet where
  parseJSON = withObject "Spritesheet" $ \obj -> newSpritesheet
    <$> obj .: "texture"
    <*> (V2 <$> obj .: "cellWidth" <*> obj .: "cellHeight")
    <*> (V2 <$> obj .: "gridWidth" <*> obj .: "gridHeight")