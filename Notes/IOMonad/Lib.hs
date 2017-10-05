module Lib
	( intro,
	  add,
	  mult
	) where

intro :: IO()
intro = putStrLn "Input 2 numbers."

add :: String -> String -> IO()
add a b = putStrLn $ a ++ " + " ++ b ++ " = " ++ show ((read a :: Integer)+(read b :: Integer))

mult :: String -> String -> IO()
mult a b = putStrLn $ a ++ " * " ++ b ++ " = " ++ show ((read a :: Integer)*(read b :: Integer))