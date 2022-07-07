module Testing.Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)
import Testing.Morse (morseToChar, stringToMorse)

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just s) -> putStrLn (intercalate " " s)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  -- otherwise, proceed.
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded =
            traverse
              morseToChar
              (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

morse :: IO ()
morse = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn
        "Please specify the\
        \ first argument\
        \ as being 'from' or\
        \ 'to' morse,\
        \ such as: morse to"
      exitFailure
