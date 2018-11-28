
--("nombre pelicula", "\123 \456 \789"),

module Main(main) where

import Data.Emoji

main :: IO ()
main = do
  putStrLn "Hello World"
  line <- getLine
  let a = unicodeByName line
  mapM_ putStrLn (unicode	ByName line)
  mapM_ putStrLn (unicodeByName "pizza")
