module Main where

import MyTypes ()
import Utils ( isPPM, isPBM, isPGM, grayscale )
import IOFunctions ( readPPM, readPGM, readPBM, saveImagePPM )
import BaseDithering ( generalDithering )
import AlgoNeighbours ()
import AlgoHandler ( algoNames, getMatchingError )

-- '<-' for io things
-- other pure things 'let'
inputHandler :: IO ()
inputHandler = do  
    putStrLn "Enter input file name (.ppm, .pgm or .pbm): "
    fileName <- getLine
    content <- readFile fileName

    putStrLn "Available algorithms:"
    putStrLn algoNames
    putStrLn "Enter wanted algorithm number: "
    algoNumber <- getLine

    let algoErrors = getMatchingError (read algoNumber)

    putStrLn "Enter output file name (same extension as input): "
    outputName <- getLine

    if isPPM fileName && isPPM outputName
        then saveImagePPM outputName $ generalDithering (grayscale $ readPPM $ lines content) algoErrors
            else 
                if isPGM fileName && isPGM outputName
                    then saveImagePPM outputName $ generalDithering (grayscale $ readPGM $ lines content) algoErrors
                    else 
                       if isPBM fileName && isPBM outputName
                           then saveImagePPM outputName $ generalDithering (grayscale $ readPBM $ lines content) algoErrors
                           else putStrLn "Invalid input/output file format!"

main :: IO ()
main = inputHandler
