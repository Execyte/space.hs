module Game.Client.Renderer.RectPacker (
  Packer,
  newPacker,
  -- packRect,
  -- packRects
  packRect
) where

-- import Data.List
-- import Data.Ord (Down(Down))

import Linear

data Packer = Packer { px, py, pw, ph, lh :: Int }

newPacker :: Int -> Int -> Packer
newPacker w h = Packer 0 0 w h 0

packRect :: Packer -> V2 Int -> (Packer, V4 Int)
packRect packer rect =
  let
    (V2 rw rh) = rect
    (Packer {..}) = packer
    lh' = max rh lh
    (px', py', lh'') = if px + rw >= pw then (0, lh', 0) else (px + rw, py, lh')
  in
    (packer { px = px', py = py', lh = lh'' }, V4 px py rw rh)

-- packRects :: Packer -> [Rect] -> (Packer, [Rect])
-- packRects packer = mapAccumL packRect packer . sortByHeight
--   where
--     sortByHeight = sortBy $ \(Rect _ _ _ a) (Rect _ _ _ b) -> compare (Down a) (Down b)
