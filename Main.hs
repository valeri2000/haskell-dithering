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
import Control.Exception ( SomeException, catch )

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
                    then catch (saveImagePPM outputName $ execAlgo (read algoNumber) (grayscale $ readPPM $ lines content)) handler
                        else 
                            if isPGM fileName && isPGM outputName
                                then catch (saveImagePGM outputName $ execAlgo (read algoNumber) (grayscale $ readPGM $ lines content)) handler
                                else 
                                if isPBM fileName && isPBM outputName
                                    then catch (saveImagePBM outputName $ execAlgo (read algoNumber) (grayscale $ readPBM $ lines content)) handler
                                    else putStrLn "Invalid output file name!"
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn "\nInvalid content format in input image!"

main :: IO ()
main = inputHandler
