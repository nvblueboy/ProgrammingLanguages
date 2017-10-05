module Main where

import Lib

main :: IO()
main = do
	intro
	a <- getLine
	b <- getLine
	add a b
	mult a b