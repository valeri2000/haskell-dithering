module BaseDithering where

import MyTypes
    ( Error, Image(Image, rows, pixels, cols), Pixel(Pixel, r) )
import Utils
    ( chunks, clamp0255, makeMatrix, updateErrors, updateMatrix )

generalDithering :: Image -> [Error] -> Image
generalDithering img neigh = Image (rows img) (cols img) $ help 0 0 neigh (head $ pixels img) (tail $ pixels img) [] (makeMatrix 3 (cols img))
    where help x y errors row rows res errorMatrix
                    | null row && null rows = chunks (cols img) (reverse res)
                    | null row = help (x + 1) 0 errors (head rows) (tail rows) res errorMatrix
                    | otherwise = help x (y + 1) errors (tail row) rows ((Pixel newColor newColor newColor) : res) (updateErrors (updateMatrix errorMatrix 0 (x `mod` 3) y) errors currError x y)
                                 where elem = head row
                                       elemColor = clamp0255((r elem) + errorMatrix!!(x `mod` 3)!!y) -- RESET TO 0
                                       newColor = if elemColor > 127 then 255 else 0
                                       currError = elemColor - newColor
--newRes = res ++ [Pixel newColor newColor newColor]