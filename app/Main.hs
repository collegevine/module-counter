module Main where

import Lib

import Control.Monad (mapM_)

main :: IO ()
main = do
    modules <- runIO program
    print $ length modules
    mapM_ print modules
