module Main where

import           AMUtil

main :: IO ()
main = putStrLn . show . average $ [1 .. 10]
-- >>> main
-- Just 5.5
