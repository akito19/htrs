module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import HTRS

main :: IO ()
main = do
    filename : _ <- getArgs
    s <- readFile filename
    case parse parseTRS filename s of
      Right trs -> putStrLn $ showResult trs
      Left  e   -> print e 
