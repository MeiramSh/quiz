module Main (main) where

import Control.Monad (foldM, unless, when, (>=>))
import Data.List (delete)
import Data.List.Split (splitOn)
import GHC.IO.Encoding (TextEncoding (textEncodingName))
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import System.Random (Random (randomR), initStdGen)

parse = map (toTuple . splitOn ": ") . filter (elem ':') . splitOn "\n\n"
  where
    toTuple [a, b] = (a, b)

check ans resp
  | ans == resp = "✔ Correct!"
  | otherwise = "✗ Incorrect, the right answer is " ++ ans

main = do
  gen <- initStdGen
  home <- getEnv "HOME"
  notes <- readFile $ home ++ "/Documents/Spring 2023/BIOL 110/notes.txt"
  quiz 1 gen . parse $ init notes -- to remove \r in the end

prompt text = do
  putStr text
  hFlush stdout
  getLine

quiz n gen questions =
  unless (null questions || n > 25) $ do
    let (i, g) = randomR (0, length questions - 1) gen
    let (question, ans) = questions !! i
    putStrLn $ show n ++ ". " ++ question
    resp <- prompt "Your answer: "
    when (resp /= "q") $ do
      putStrLn $ "    " ++ check ans resp ++ "\n"
      quiz (n + 1) g $ delete (question, ans) questions
