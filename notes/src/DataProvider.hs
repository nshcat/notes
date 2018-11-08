module DataProvider
    ( DataProvider(..)
    ) where
   
import System.IO
import Note
   
class DataProvider a where
    saveNote     :: a -> Note -> IO ()
    deleteNote   :: a -> Int -> IO ()
    retrieveNote :: a -> Int -> IO (Maybe(Note))
    nextNotes    :: a -> Int -> IO([Note])
    allNotes     :: a -> IO([Note])
    hasNote      :: a -> Int -> IO(Bool)
    updateNote   :: a -> Int -> Note -> IO()
    exists       :: a -> IO(Bool)
    create       :: a -> IO()
