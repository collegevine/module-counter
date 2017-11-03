module Main where

import Lib

main :: IO ()
main = do
    modules <- runIO program
    print $ length modules
    print modules

