module OrderedDithering where

import MyTypes
  ( Image (Image, cols, pixels, rows),
    Pixel (Pixel, r),
  )
import Utils (chunks)

orderedDithering :: Image -> [[Double]] -> Int -> Image
orderedDithering img matrix modMatrix = Image (rows img) (cols img) $ help (pixels img) matrix 0 0 (rows img) (cols img) []
  where
    help img matrix x y n m res
      | x == n = chunks m $ reverse res
      | y == m = help img matrix (x + 1) 0 n m res
      | otherwise =
        if myRatio < mapRatio
          then (help img matrix x (y + 1) n m $ (Pixel 0 0 0) : res)
          else (help img matrix x (y + 1) n m $ (Pixel 255 255 255) : res)
      where
        myRatio = fromIntegral (r (img !! x !! y)) / 255
        mapRatio = matrix !! (x `mod` modMatrix) !! (y `mod` modMatrix) :: Double