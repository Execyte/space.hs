module Client.RectPack (
  Packer(..),
  Rect(..),
  makePacker,
  packRect,
  packRects
) where

import Data.List
import Data.Ord (Down(Down))

data Packer = Packer {
  px, py :: Int,
  pw, ph :: Int,
  lh :: Int
}

makePacker :: Int -> Int -> Packer
makePacker w h = Packer 0 0 w h 0

data Rect = Rect {
  x, y, w, h :: Int
} deriving Show

packRect :: Packer -> Rect -> (Packer, Rect)
packRect packer rect =
  let
    (Rect _ _ rw rh) = rect
    (Packer {..}) = packer
    lh' = max rh lh
    (px', py', lh'') = if px + rw >= pw then (0, lh', 0) else (px + rw, py, lh')
  in
    (packer { px = px', py = py', lh = lh'' }, rect { x = px, y = py })

packRects :: Packer -> [Rect] -> (Packer, [Rect])
packRects packer = mapAccumL packRect packer . sortByHeight
  where
    sortByHeight = sortBy $ \(Rect _ _ _ a) (Rect _ _ _ b) -> compare (Down a) (Down b)