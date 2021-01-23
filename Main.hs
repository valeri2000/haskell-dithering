module Main where

import MyTypes ()
import Utils (isStringNumb,  grayscale, isPBM, isPGM, isPPM ) 
import IOFunctions
    ( readPBM,
      readPGM,
      readPPM,
      saveImagePBM,
      saveImagePGM,
      saveImagePPM )
import BaseDithering ()
import AlgoNeighbours ()
import AlgoHandler ( algoNames, execAlgo )
import OrderedDithering ()
import BayerMatrices ()

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

    if not (isStringNumb algoNumber) || read algoNumber > 10 
        then putStrLn "Invalid input! You should enter a number in the range [0; 10]!"
            else
                do
                putStrLn "\nEnter output file name (same extension as input): "
                outputName <- getLine

                if isPPM fileName && isPPM outputName
                    then saveImagePPM outputName $ execAlgo (read algoNumber) (grayscale $ readPPM $ lines content) 
                        else 
                            if isPGM fileName && isPGM outputName
                                then saveImagePGM outputName $ execAlgo (read algoNumber) (grayscale $ readPGM $ lines content) 
                                else 
                                if isPBM fileName && isPBM outputName
                                    then saveImagePBM outputName $ execAlgo (read algoNumber) (grayscale $ readPBM $ lines content) 
                                    else putStrLn "Invalid output file name!"
                
                putStrLn "\nSuccess! Check output file!"

main :: IO ()
main = inputHandler
