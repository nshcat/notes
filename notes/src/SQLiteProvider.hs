{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module SQLiteProvider
    ( SQLiteProvider
    , mkSQLiteProvider
    ) where
    
import System.IO
import Note
import Data.Maybe
import DataProvider
import Data.Time.Clock
import Data.Time.Calendar
import Database.Selda
import Database.Selda.SQLite

data Priority_ = High | Normal | Low
  deriving (Show, Read, Bounded, Enum)
instance SqlType Priority_

data Note_ = Note_
  { nid      :: ID Note_
  , priority :: Priority_
  , content  :: Text
  , dueDate  :: UTCTime
  } deriving (Show, Generic)
instance SqlRow Note_

notes :: Table Note_
notes = table "notes" [#nid :- autoPrimary]

newtype SQLiteProvider = SQLiteProvider String

instance DataProvider SQLiteProvider where
    saveNote p n = saveNote_ p $ fromNote n
    
    deleteNote = deleteNote_
    
    nextNotes p c = do
        ns <- nextNotes_ p c
        return $ map toNote $ ns
        
    allNotes p = do
        ns <- allNotes_ p
        return $ map toNote $ ns
        
    hasNote = hasNote_
    
    exists = exists_
    
    create = create_
    
    updateNote p i n = do
        let n' = fromNote n
        updateNote_ p i n'          
    
    retrieveNote p i = do
        n <- retrieveNote_ p i
        return $ maybe Nothing (Just . toNote) $ n


mkSQLiteProvider :: String -> SQLiteProvider
mkSQLiteProvider = SQLiteProvider

toNote :: Note_ -> Note
toNote Note_{..} = Note (fromId nid) (toPriority priority) content dueDate

fromNote :: Note -> Note_
fromNote Note{..} = Note_ def (fromPriority priority) content dueDate

toPriority :: Priority_ -> Priority
toPriority SQLiteProvider.High = Note.High
toPriority SQLiteProvider.Normal = Note.Normal
toPriority SQLiteProvider.Low = Note.Low

fromPriority :: Priority -> Priority_
fromPriority Note.High = SQLiteProvider.High
fromPriority Note.Normal = SQLiteProvider.Normal
fromPriority Note.Low = SQLiteProvider.Low


exists_ :: SQLiteProvider -> IO(Bool)
exists_ (SQLiteProvider p) = return False

create_ :: SQLiteProvider -> IO()
create_ (SQLiteProvider p) = withSQLite p $ tryCreateTable notes

saveNote_ :: SQLiteProvider -> Note_ -> IO()
saveNote_ (SQLiteProvider p) n = withSQLite p $ insert_ notes [n]
    
retrieveNote_ :: SQLiteProvider -> Int -> IO(Maybe(Note_))
retrieveNote_ (SQLiteProvider p) i = withSQLite p $ do
    n <- query $ do
        note <- select notes
        restrict (note ! #nid .== literal (toId i))
        return note
        
    if null n
    then return Nothing
    else return . Just . head $ n
    
nextNotes_ :: SQLiteProvider -> Int -> IO([Note_])
nextNotes_ (SQLiteProvider p) count = withSQLite p $ do
    n <- query $ do
        limit 0 count $ do  
            note <- select notes
            order (note ! #dueDate) ascending
            return note
            
    return n
    
allNotes_ :: SQLiteProvider -> IO([Note_])
allNotes_ (SQLiteProvider p) = withSQLite p $ query $ select notes

hasNote_ :: SQLiteProvider -> Int -> IO(Bool)
hasNote_ (SQLiteProvider p) i = withSQLite p $ do
    n <- query $ do
        note <- select notes
        restrict (note ! #nid .== literal (toId i))
        return note
    
    return $ (not . null) $ n 
    
    
updateNote_ :: SQLiteProvider -> Int -> Note_ -> IO()
updateNote_ (SQLiteProvider p) i Note_{..} = withSQLite p $ do
    update notes
        (\note -> (note ! #nid .== literal (toId i)))
        (\note -> note `with` [#priority := literal priority, #content := literal content, #dueDate := literal dueDate])
    return ()
            

deleteNote_ :: SQLiteProvider -> Int -> IO()
deleteNote_ (SQLiteProvider p) i = withSQLite p $ do
    deleteFrom notes (\note -> (note ! #nid .== literal (toId i)))
    return ()

