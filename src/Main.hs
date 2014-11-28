module Main where

import ColorSound (colorSound)

import Graphics.Gloss.Interface.Pure.Animate (animate, black, Display(..))
import Data.WAVE (getWAVEFile)
import System.Process (runCommand)

audioFile :: String
audioFile = "sample.wav"

main :: IO ()
main = do
    wav <- getWAVEFile audioFile
    runCommand $ "aplay " ++ audioFile
    animate 
        (InWindow "ColorSound" (1366,768) (150,150)) 
        black
        (colorSound wav)
