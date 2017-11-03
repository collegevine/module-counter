module Distribution.Modules
    (
        libraryModules,
        CabalFilePath
    )
where

import Distribution.ModuleName (components)
import Distribution.Package (Dependency)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (normal)
import Data.Maybe (maybe)

type CabalFilePath = String
type Module = [String]

libraryModules ::
    CabalFilePath
    -> IO [[String]]
libraryModules =
    fmap (maybe [] extractModules . condLibrary) . readPackageDescription normal

    where
    extractModules =
        fmap components . exposedModules . condTreeData

-- Note this can easily be extended to work on executables etc...
