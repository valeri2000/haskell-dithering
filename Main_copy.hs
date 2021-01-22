module Main where

import Data.Word (Word8)
import Data.List

data Pixel = Pixel {
    r :: Int,
    g :: Int,
    b :: Int
} deriving (Show, Eq)

data Image = NullImage | Image {
    rows :: Int,
    cols :: Int,
    pixels :: [[Pixel]]
} deriving (Show, Eq)

data Error = Error {
    offsetx :: Int,
    offsety :: Int,
    weightnum :: Int,
    weightdenom :: Int
}

-- '<-' for io things
-- other pure things 'let'
readFileContents fileName = do  
    content <- readFile fileName
    saveImagePPM "output.ppm" $ generalDithering (grayscale $ readPPM $ lines content) floydSteinbergErrors
    -- if isPPM fileName
    --     then return $ saveImagePPM "output.txt" $ generalDithering (grayscale $ readPPM $ lines content) floydSteinbergErrors
    --         else 
    --             if isPGM fileName
    --                 then return $ readPGM $ lines content
    --                 else 
    --                    if isPBM fileName
    --                        then return $ readPBM $ lines content
    --                        else return NullImage

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

readPPM :: [String] -> Image
readPPM content = help content 0 0 []
    where help content rows cols pixelList
            | null content = if rows > 0 && cols > 0 then Image rows cols (chunks cols pixelList) else NullImage
            | length curr == 0 = help left rows cols pixelList
            | curr == "P3" = help left rows cols pixelList
            | cols == 0 = help (tail left) (fst $ stringToSize curr) (snd $ stringToSize curr) pixelList
            | otherwise = help (tail $ tail left) rows cols (pixelList ++ [stringToPixel (curr ++ " " ++ (head left) ++ " " ++ (head $ tail left))])
            where curr = head content
                  left = tail content

readPGM :: [String] -> Image
readPGM content = help content 0 0 []
    where help content rows cols pixelList
            | null content = if rows > 0 && cols > 0 then Image rows cols (chunks cols pixelList) else NullImage
            | length curr == 0 = help left rows cols pixelList
            | curr == "P2" = help left rows cols pixelList
            | cols == 0 = help (tail left) (fst $ stringToSize curr) (snd $ stringToSize curr) pixelList
            | otherwise = help left rows cols (pixelList ++ [Pixel (read numb) (read numb) (read numb) | numb <- words curr])
            where curr = head content
                  left = tail content

whiteBlackToPixel :: Int -> Pixel
whiteBlackToPixel value = Pixel conv conv conv
    where conv = 255 * (1 - value)

readPBM :: [String] -> Image
readPBM content = help content 0 0 []
    where help content rows cols pixelList
            | null content = if rows > 0 && cols > 0 then Image rows cols (chunks cols pixelList) else NullImage
            | length curr == 0 = help left rows cols pixelList
            | curr == "P1" = help left rows cols pixelList
            | cols == 0 = help (tail left) (fst $ stringToSize curr) (snd $ stringToSize curr) pixelList
            | otherwise = help left rows cols (pixelList ++ [whiteBlackToPixel (read numb) | numb <- words curr])
            where curr = head content
                  left = tail content

rgbToString :: Pixel -> String
rgbToString Pixel {r = rr, g = gg, b = bb} = (show rr) ++ " " ++ (show gg) ++ " " ++ (show bb)

convertAllToStrings :: [[Pixel]] -> [String]
convertAllToStrings arr = [rgbToString x | x <- (concat arr)]

saveImagePPM :: FilePath -> Image -> IO ()
saveImagePPM path (Image {rows = h, cols = w, pixels = c}) =
  writeFile path (unlines (["P3", show w, show h, "255"] ++ (convertAllToStrings c)))

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

updateAt :: Int -> Int -> a -> [[a]] -> [[a]]
updateAt x y value arr
    | x < 0 || y < 0 || x >= length arr || y >= (length $ head arr) = arr
    | otherwise = [[if x == cx && y == cy then value else arr !! cx !! cy | cy <- [0, 1 .. m]] | cx <- [0, 1 .. n]]
        where n = (length arr) - 1
              m = (length $ head arr) - 1

addToPixel :: Int -> Int -> Int -> [[Pixel]] -> [[Pixel]]
addToPixel x y value arr
    | x < 0 || y < 0 || x >= length arr || y >= (length $ head arr) = arr
    | otherwise = [[if x == cx && y == cy 
        then Pixel (value + r (arr !! cx !! cy)) 0 0
        else arr !! cx !! cy | cy <- [0, 1 .. m]] | cx <- [0, 1 .. n]]
        where n = (length arr) - 1
              m = (length $ head arr) - 1

clamp0255 :: Int -> Int
clamp0255 x
  | x > 255 = 255
  | x < 0 = 0
  | otherwise = x

updateErrors :: Image -> [Error] -> Int -> Int -> Int -> Image
updateErrors img [] value x y = img
updateErrors img (curr:errors) value x y = updateErrors (Image (rows img) (cols img) (addToPixel (x + dx) (y + dy) (value * mull `div` divv) (pixels img))) errors value x y
                                            where dx = offsetx curr
                                                  dy = offsety curr
                                                  mull = weightnum curr
                                                  divv = weightdenom curr

generalDithering :: Image -> [Error] -> Image
generalDithering img neigh = Image (rows img) (cols img) $ auxFunc img neigh [] 0 0
    where auxFunc img neigh res x y
            | x == rows img = chunks (cols img) res
            | y == cols img = auxFunc img neigh res (x + 1) 0
            | otherwise = auxFunc (updateErrors img neigh curr_error x y) neigh new_res x (y+1)
                where color = clamp0255 $ r ((pixels img) !! x !! y)
                      new_value = if color > 128 then 255 else 0
                      curr_error = color - new_value
                      new_res = res ++ [Pixel new_value new_value new_value]

floydSteinbergErrors :: [Error]
floydSteinbergErrors = [Error 0 1 7 16,
                        Error 1 (negate 1) 3 16,
                        Error 1 0 5 16,
                        Error 1 1 1 16]

main :: IO ()
main = return ()
