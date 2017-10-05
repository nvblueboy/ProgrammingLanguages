module Main where

import Data.Char
import System.Environment

main :: IO()
main = do
    args <- getArgs
    let fileName = head args
    b <- readFile fileName
    putStrLn $ caps b

    a <- getLine
    putStrLn $ caps a

caps :: String -> String
caps x = if (length x) == 0 then "" else [(toUpper (head x))] ++ (caps (drop 1 x))