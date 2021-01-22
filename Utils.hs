module Utils where

import MyTypes
    ( Error(offsetx, offsety, weightnum, weightdenom),
      Image(Image, rows, cols, pixels),
      Pixel(..) )

import Data.List ( isSuffixOf )

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = ys : chunks n zs
    where (ys, zs) = splitAt n xs

stringToPixel :: String -> Pixel
stringToPixel str = Pixel (read $ numb !! 0) (read $ numb !! 1) (read $ numb !! 2)
    where numb = words str

stringToSize :: String -> (Int, Int)
stringToSize str = (read $ numb !! 1, read $ numb !! 0)
    where numb = words str

isPPM :: FilePath -> Bool
isPPM fileName = isSuffixOf ".ppm" fileName

isPBM :: FilePath -> Bool
isPBM fileName = isSuffixOf ".pbm" fileName

isPGM :: FilePath -> Bool
isPGM fileName = isSuffixOf ".pgm" fileName

whiteBlackToPixel :: Int -> Pixel
whiteBlackToPixel value = Pixel conv conv conv
    where conv = 255 * (1 - value)

ppmPixelToString :: Pixel -> String
ppmPixelToString Pixel {r = rr, g = gg, b = bb} = (show rr) ++ " " ++ (show gg) ++ " " ++ (show bb)

pgmPixelToString :: Pixel -> String
pgmPixelToString Pixel {r = rr, g = gg, b = bb} = (show rr)

pbmPixelToString :: Pixel -> String
pbmPixelToString Pixel {r = rr, g = gg, b = bb} = if rr == 0 then "1" else "0"

convertAllToStrings :: [[Pixel]] -> String -> [String]
convertAllToStrings arr "PPM" = [ppmPixelToString x | x <- (concat arr)]
convertAllToStrings arr "PGM" = [pgmPixelToString x | x <- (concat arr)]
convertAllToStrings arr "PBM" = [pbmPixelToString x | x <- (concat arr)]

pixelToGrayscale :: Pixel -> Pixel
pixelToGrayscale (Pixel red green blue) = Pixel {
    r = new_value,
    g = new_value,
    b = new_value
} where new_value = round ((fromIntegral red) * 0.3 + (fromIntegral green) * 0.59 + (fromIntegral blue) * 0.11)

grayscale :: Image -> Image
grayscale (Image mrows mcols mcontent) = Image {
    rows = mrows,
    cols = mcols,
    pixels = [map pixelToGrayscale row | row <- mcontent]
}

clamp0255 :: Int -> Int
clamp0255 x
  | x > 255 = 255
  | x < 0 = 0
  | otherwise = x

makeMatrix :: Int -> Int -> [[Int]]
makeMatrix x y = [[0 | y1 <- [0..y]] | x1 <- [0..x]]

updateMatrix :: [[a]] -> a -> Int -> Int -> [[a]]
updateMatrix m value r c =
  take r m ++
  [take c (m !! r) ++ [value] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

reverseList xs = foldl (\x y -> y:x) [] xs 

updateErrors :: [[Int]] -> [Error] -> Int -> Int -> Int -> [[Int]]
updateErrors matrix [] value x y = matrix
updateErrors matrix (curr:errors) value x y
    | x + dx < 0 || y + dy < 0 = updateErrors matrix errors value x y
    | otherwise = updateErrors (updateMatrix matrix (matrix!!((x+dx) `mod` 3)!!(y+dy) + value * mull `div` divv) ((x+dx) `mod` 3) (y+dy)) errors value x y
                                            where dx = offsetx curr
                                                  dy = offsety curr
                                                  mull = weightnum curr
                                                  divv = weightdenom curr