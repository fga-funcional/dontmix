module Database
    ( findOrCreatePageJSON,
      editPageJSON
    ) where

import System.IO
import System.Directory

findOrCreatePageJSON :: FilePath -> IO String
findOrCreatePageJSON path = do
    let filePath = "./db/" ++ path ++ ".json"

    createDirectoryIfMissing False  "./db/"
    h <- openFile filePath ReadWriteMode
    raw_contents <- hGetContents h
    startJSONWithDefault filePath raw_contents
    return filePath
    
startJSONWithDefault :: FilePath -> String -> IO ()
startJSONWithDefault path "" = writeFile path "{ \"Selected\": {} \"Recommended\": {} }" 
startJSONWithDefault path _ = mempty

editPageJSON :: FilePath -> String -> IO String
editPageJSON path contents = do
    let filePath = "./db/" ++ path ++ ".json"
    writeFile filePath contents
    return filePath
