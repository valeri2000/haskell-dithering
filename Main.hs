module Main where

import AlgoHandler (algoNames, execAlgo)
import AlgoNeighbours ()
import BaseDithering ()
import BayerMatrices ()
import Control.Exception (SomeException, catch)
import IOFunctions
  ( readPBM,
    readPGM,
    readPPM,
    saveImagePBM,
    saveImagePGM,
    saveImagePPM,
  )
import MyTypes ()
import OrderedDithering ()
import Utils (grayscale, isPBM, isPGM, isPPM, isStringNumb)

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
        else do
          putStrLn "\nEnter output file name (same extension as input): "
          outputName <- getLine

          if not (isPPM outputName) && not (isPBM outputName) && not (isPGM outputName)
            then putStrLn "Invalid output file name!"
            else
              if isPPM fileName
                then
                  if isPPM outputName
                    then catch (saveImagePPM outputName $ execAlgo (read algoNumber) (grayscale $ readPPM $ lines content)) handler
                    else
                      if isPBM outputName
                        then catch (saveImagePBM outputName $ execAlgo (read algoNumber) (grayscale $ readPPM $ lines content)) handler
                        else catch (saveImagePGM outputName $ execAlgo (read algoNumber) (grayscale $ readPPM $ lines content)) handler
                else
                  if isPGM fileName
                    then
                      if isPPM outputName
                        then catch (saveImagePPM outputName $ execAlgo (read algoNumber) (grayscale $ readPGM $ lines content)) handler
                        else
                          if isPBM outputName
                            then catch (saveImagePBM outputName $ execAlgo (read algoNumber) (grayscale $ readPGM $ lines content)) handler
                            else catch (saveImagePGM outputName $ execAlgo (read algoNumber) (grayscale $ readPGM $ lines content)) handler
                    else
                      if isPPM outputName
                        then catch (saveImagePPM outputName $ execAlgo (read algoNumber) (grayscale $ readPBM $ lines content)) handler
                        else
                          if isPBM outputName
                            then catch (saveImagePBM outputName $ execAlgo (read algoNumber) (grayscale $ readPBM $ lines content)) handler
                            else catch (saveImagePGM outputName $ execAlgo (read algoNumber) (grayscale $ readPBM $ lines content)) handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn "\nError while reading file! Invalid input image format!"

main :: IO ()
main = inputHandler
