{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    (   program
      , runIO
      , prettyPrinter
    ) where

import Distribution.Modules (libraryModules, CabalFilePath)
import Distribution.Packages (listProjectPackages)

import Control.Exception (catch, IOException)
import Control.Monad.Free
import Data.Monoid ((<>))
import Data.String (IsString)
import qualified Data.Text as T
import System.Process (readProcess)

--
-- IO Interpreter
--

runIO ::
    StackAPI a ->
    IO a
runIO =
    iterM interpStack
    where
        interpStack :: StackF (IO a) -> IO a
        interpStack (ListPackages rest) =
            rest =<< (parsePackages <$> listProjectPackages)

        interpStack (ListModules p rest) =
            rest =<< (fmap concat . sequence $ safeModules <$> naiveCabalPath p)
            where
            safeModules path =
                catch
                    (makeModule <$> libraryModules path)
                    (\ (e :: IOException) -> pure [])

prettyPrinter ::
    (IsString a, Monoid a) =>
    StackAPI a ->
    a
prettyPrinter =
    iter interpStack
    where
        interpStack :: (IsString a, Monoid a) =>
            StackF a
            -> a

        interpStack (ListPackages rest) =
            "ListPackages \n" <> (rest [Package "dummy"])
        interpStack (ListModules p rest) =
            "ListModules \n" <> (rest [])


parsePackages ::
    [String] ->
    [Package]
parsePackages = fmap (Package . T.pack)

-- Many dubious assumptions about project structure here
naiveCabalPath ::
    Package ->
    [CabalFilePath]
naiveCabalPath (Package p) =
    [
        "./" <> T.unpack p <> "/" <> T.unpack p <> "/" <> T.unpack p <> ".cabal"
        ,"./" <> T.unpack p <> "/" <> T.unpack p <> ".cabal"
        ,"./" <> T.unpack p <> ".cabal"
    ]

program :: StackAPI [Module]
program =
    (fmap concat . traverse listModules) =<< listPackages

type StackAPI = Free StackF
newtype Package = Package T.Text
    deriving Show
newtype Module = Module [T.Text]
    deriving Show

data StackF a
    = ListPackages ([Package] -> a)
    | ListModules Package ([Module] -> a)

instance Functor StackF where
    fmap f (ListPackages rest) = ListPackages $ fmap f rest
    fmap f (ListModules p rest) = ListModules p $ fmap f rest

listPackages ::
    StackAPI [Package]
listPackages =
    liftF $ ListPackages id

listModules ::
    Package ->
    StackAPI [Module]
listModules p =
    liftF $ ListModules p id

makeModule ::
    [[String]]
    -> [Module]
makeModule = fmap (Module . (fmap T.pack))
