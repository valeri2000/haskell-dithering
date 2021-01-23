module AlgoHandler where

import MyTypes
import BaseDithering
import OrderedDithering
import BayerMatrices
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
            \8 - Sierra Lite Dithering\n\
            \9 - Bayer 4x4 Dithering\n\
            \10 - Bayer 8x8 Dithering\n"

execAlgo :: Int -> Image -> Image
execAlgo x img
    | x == 1 = generalDithering img floydSteinbergErrors 
    | x == 2 = generalDithering img jarvisJudiceNinkeErrors  
    | x == 3 = generalDithering img stuckiErrors   
    | x == 4 = generalDithering img atkinsonErrors   
    | x == 5 = generalDithering img burkesErrors   
    | x == 6 = generalDithering img sierraErrors   
    | x == 7 = generalDithering img sierraTwoRowErrors   
    | x == 8 = generalDithering img sierraLiteErrors   
    | x == 9 = orderedDithering img bayer4x4 4
    | x == 10 = orderedDithering img bayer8x8 8
    | otherwise = NullImage