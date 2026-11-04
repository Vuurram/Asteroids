module HighScore where

import System.IO
import System.Directory(doesFileExist)
import Model


initializeHighScoreFile :: FilePath -> IO ()
initializeHighScoreFile filePath = do
    exist <- doesFileExist filePath
    if not exist
       then writeFile filePath "HighScore: 0"  
       else return ()

getHighScore :: FilePath -> IO Score
getHighScore filePath = do
    initializeHighScoreFile filePath
    content <- readFile filePath
    let score = case words content of 
                  (_:num:_) -> read num
                  _         -> 0
    return score

updateHighScore :: FilePath -> Score -> IO Score
updateHighScore filePath newScore = do
    currentHighScore <- getHighScore filePath
    if newScore > currentHighScore
      then do
        writeFile filePath ("HighScore: " ++ show newScore)
        return newScore
      else return currentHighScore





