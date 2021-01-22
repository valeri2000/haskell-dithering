module IOFunctions where

import MyTypes ( Image(..), Pixel(Pixel) )
import Utils
    ( chunks,
      convertAllToStrings,
      stringToPixel,
      stringToSize,
      whiteBlackToPixel )

readPPM :: [String] -> Image
readPPM content = help content 0 0 []
    where help content rows cols pixelList
            | null content = if rows > 0 && cols > 0 then Image rows cols (chunks cols $ reverse pixelList) else NullImage
            | length curr == 0 = help left rows cols pixelList
            | curr == "P3" = help left rows cols pixelList
            | cols == 0 = help (tail left) (fst $ stringToSize curr) (snd $ stringToSize curr) pixelList
            | otherwise = help (tail $ tail left) rows cols $ (stringToPixel (curr ++ " " ++ (head left) ++ " " ++ (head $ tail left))) : pixelList
            where curr = head content
                  left = tail content
-- | otherwise = help (tail $ tail left) rows cols (pixelList ++ [stringToPixel (curr ++ " " ++ (head left) ++ " " ++ (head $ tail left))])

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


saveImagePPM :: FilePath -> Image -> IO ()
saveImagePPM path (Image {rows = h, cols = w, pixels = c}) = 
  writeFile path (unlines (["P3", show w, show h, "255"] ++ (convertAllToStrings c "PPM")))

saveImagePBM :: FilePath -> Image -> IO ()
saveImagePBM path (Image {rows = h, cols = w, pixels = c}) = 
  writeFile path (unlines (["P1", show w, show h] ++ (convertAllToStrings c "PBM")))

saveImagePGM :: FilePath -> Image -> IO ()
saveImagePGM path (Image {rows = h, cols = w, pixels = c}) = 
  writeFile path (unlines (["P2", show w, show h, "255"] ++ (convertAllToStrings c "PGM")))

