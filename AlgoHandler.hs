module AlgoHandler where

import MyTypes ( Error )
import AlgoNeighbours
    ( floydSteinbergErrors,
      jarvisJudiceNinkeErrors,
      stuckiErrors,
      atkinsonErrors,
      burkesErrors,
      sierraErrors,
      sierraTwoRowErrors,
      sierraLiteErrors,
      invalidErrors )

algoNames :: String
algoNames = "1 - Floyd-Steinberg Dithering\n\
            \2 - Jarvis, Judice, and Ninke Dithering\n\
            \3 - Stucki Dithering\n\
            \4 - Atkinson Dithering\n\
            \5 - Burkes Dithering\n\
            \6 - Sierra Dithering\n\
            \7 - Two-Row Sierra Dithering\n\
            \8 - Sierra Lite Dithering\n"

getMatchingError :: Int -> [Error]
getMatchingError x
    | x == 1 = floydSteinbergErrors 
    | x == 2 = jarvisJudiceNinkeErrors  
    | x == 3 = stuckiErrors   
    | x == 4 = atkinsonErrors   
    | x == 5 = burkesErrors   
    | x == 6 = sierraErrors   
    | x == 7 = sierraTwoRowErrors   
    | x == 8 = sierraLiteErrors   
    | otherwise = invalidErrors