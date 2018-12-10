module Database
    ( findOrCreatePageJSON
    , editPageJSON
    )
where

import qualified System.IO as IO
import qualified System.IO.Strict as Strict
import System.Directory

findOrCreatePageJSON :: FilePath -> IO String
findOrCreatePageJSON path = do
    let filePath = "./db/" ++ path ++ ".json"

    createDirectoryIfMissing False "./db/"
    h <- IO.openFile filePath IO.ReadWriteMode
    contents <- Strict.hGetContents h
    startJSONWithDefault filePath contents
    return filePath

startJSONWithDefault :: FilePath -> String -> IO ()
startJSONWithDefault path "" =
    writeFile path "{ \"Selected\": [], \"Recommended\": [] }"
startJSONWithDefault path _ = mempty

editPageJSON :: FilePath -> String -> IO String
editPageJSON path contents = do
    let filePath = "./db/" ++ path ++ ".json"
    writeFile filePath contents
    return filePath
