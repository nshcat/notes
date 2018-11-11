{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration
    ( retrieveConfig
    , Configuration(..)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad
import System.IO
import System.Directory
import System.FilePath
import ApplicationPaths
import qualified Data.ByteString.Lazy.Char8 as B

data Configuration = Configuration {
      databaseName :: String
    } deriving (Show, Generic, ToJSON, FromJSON)
    
defaultConfig :: Configuration
defaultConfig = Configuration "notes.sqlite"

configFileName :: FilePath
configFileName = "config.json"
    
retrieveConfig :: IO(Configuration)
retrieveConfig = do
    root <- configRootDir
    let path = root </> configFileName
    exists <- doesFileExist path
    if (not exists)
    then
         createConfig
    else do
        contents <- readFile path
        let result = decode . B.pack $ contents
        case result of
            (Just c) -> return c
            (Nothing) -> do
                putStrLn "warning: configuration file is corrupted. using defaults"
                return defaultConfig
        

-- This overwrites any preexisting configuration file
createConfig :: IO(Configuration)
createConfig = do
    root <- configRootDir
    let path = root </> configFileName
    let c = defaultConfig
    
    -- If a directory with the same name as the config file exists, remove that.
    result <- doesDirectoryExist path
    when result $ removeDirectory path
    
    
    writeFile (root </> configFileName) $ (B.unpack . encodePretty $ c)
    return c
