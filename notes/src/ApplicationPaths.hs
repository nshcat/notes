module ApplicationPaths
    ( configRootDir
    ) where
    
import System.Directory
import System.IO

configRootDir :: IO (FilePath)
configRootDir = do
    dir <- getXdgDirectory XdgConfig "notes"
    
    -- Create the directory if it does not exist yet
    createDirectoryIfMissing True dir
    return dir
