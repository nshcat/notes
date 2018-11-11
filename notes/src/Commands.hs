{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Commands
    ( Command(..)
    , NewNoteCommand(..)
    ) where
    

import Application
import Note
import ApplicationPaths
import Configuration
import DataProvider
import SQLiteProvider
import Options.Applicative
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text
import Control.Monad.Reader
import Control.Monad.Morph
import Control.Monad.Identity
import System.FilePath


data Command = NewNote NewNoteCommand 
             | DeleteNote DeleteNoteCommand


data NewNoteCommand = NewNoteCommand {
      content   :: Text
    , dueDate   :: UTCTime
    , priority  :: Priority
    } deriving (Show)
    
data DeleteNoteCommand = DeleteNoteCommand {
      id :: Int
    } deriving (Show)
    

createDBProvider ::Application Configuration (SQLiteProvider)
createDBProvider = do
    cfg <- ask
    root <- liftIO $ configRootDir
    return . mkSQLiteProvider $ root </> (databaseName cfg)
    

runNewNote :: NewNoteCommand -> Application Configuration ()
runNewNote NewNoteCommand{..} = do
    p <- createDBProvider
    let note = Note noId priority content dueDate
    liftIO $ saveNote p note
    
runDeleteNote :: DeleteNoteCommand -> Application Configuration ()
runDeleteNote DeleteNoteCommand{..} = do
    p <- createDBProvider
    liftIO $ deleteNote p id
    
    
