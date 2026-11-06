
module HighScore where

import System.IO
import System.Directory (doesFileExist)
import Control.Exception (evaluate)
import Text.Read (readMaybe)
import Model

-- Initialize the high score file if it doesn't exist
initializeHighScoreFile :: FilePath -> IO ()
initializeHighScoreFile filePath = do
    exist <- doesFileExist filePath
    if not exist
       then writeFile filePath "HighScore: 0"
       else return ()

-- Get the current high score from the file
getHighScore :: FilePath -> IO Score
getHighScore filePath = do
    initializeHighScoreFile filePath
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        _ <- evaluate (length content)
        let result = case words content of
                       (_:num:_) -> maybe 0 id (readMaybe num)
                       _         -> 0
        evaluate result

-- Update the high score in the file if the new score is higher than current highscore
updateHighScore :: FilePath -> Score -> IO Score
updateHighScore filePath newScore = do
    currentHighScore <- getHighScore filePath
    if newScore > currentHighScore
      then do
        withFile filePath WriteMode $ \handle ->
            hPutStr handle ("HighScore: " ++ show newScore)
        return newScore
      else return currentHighScore
