module Main where

import MyTypes ()
import Utils ( isPPM, isPBM, isPGM, grayscale )
import IOFunctions
    ( readPPM,
      readPGM,
      readPBM,
      saveImagePPM,
      saveImagePBM,
      saveImagePGM )
import BaseDithering ( generalDithering )
import AlgoNeighbours ()
import AlgoHandler ( algoNames, getMatchingError)

-- '<-' for io things
-- other pure things 'let'
inputHandler :: IO ()
inputHandler = do  
    putStrLn "Enter input file name (.ppm, .pgm or .pbm): "
    fileName <- getLine

    if not (isPPM fileName) && not (isPBM fileName) && not (isPGM fileName)
        then putStrLn "Invalid input file name!"
            else do

    content <- readFile fileName
    putStrLn "\nAvailable algorithms:"
    putStrLn algoNames
    putStrLn "Enter wanted algorithm number: "
    algoNumber <- getLine

    let algoErrors = getMatchingError (read algoNumber)

    if null algoErrors
        then putStrLn "Invalid algorithm!"
            else
                do
                putStrLn "\nEnter output file name (same extension as input): "
                outputName <- getLine

                if isPPM fileName && isPPM outputName
                    then saveImagePPM outputName $ generalDithering (grayscale $ readPPM $ lines content) algoErrors
                        else 
                            if isPGM fileName && isPGM outputName
                                then saveImagePGM outputName $ generalDithering (grayscale $ readPGM $ lines content) algoErrors
                                else 
                                if isPBM fileName && isPBM outputName
                                    then saveImagePBM outputName $ generalDithering (grayscale $ readPBM $ lines content) algoErrors
                                    else putStrLn "Invalid output file name!"
                
                putStrLn "\nSuccess! Check output file!"

main :: IO ()
main = inputHandler
