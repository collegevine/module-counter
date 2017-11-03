module Distribution.Packages (
    listProjectPackages
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import System.Process (readProcess)

listProjectPackages :: IO [String]
listProjectPackages = do
    raw <- readProcess "stack" ["query"] ""
    case runParser pkgParser () "packages" raw of
        Left e -> error $ show e ++"\n"++ raw
        Right res -> pure . fmap name $ filter onlyOwn res
    where
        onlyOwn = not . inStackWork ".stack-work". path
            where
                inStackWork _ "" = False
                inStackWork "" p = True
                inStackWork (x:xs) (p:rest)
                    | x == p = inStackWork xs rest
                    | otherwise = inStackWork ".stack-work" rest

data PackageRec =
    PR {name :: String, path :: String, version:: String}

pkgParser :: Parser [PackageRec]
pkgParser = do
    _ <- string "locals:"
    _ <- endOfLine
    res <- pkgRec `sepBy1` (try . lookAhead $ endOfLine >> char ' ' >> char ' ')
    _ <- many endOfLine
    _ <- eof
    pure res

pkgRec :: Parser PackageRec
pkgRec = do
    _ <- spaces
    name <- manyTill anyChar (char ':')
    _ <- spaces
    _ <- string "path:"
    _ <- spaces
    path <- manyTill anyChar endOfLine
    _ <- spaces
    _ <- string "version:"
    _<- spaces
    version <- manyTill anyChar (lookAhead endOfLine)
    pure $ PR name path version
