module Commandline
    ( 
    ) where
    
import Application
import Commands
import Note
import ApplicationPaths
import Configuration
import Options.Applicative
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text
import Control.Monad.Reader
import Control.Monad.Morph
import Control.Monad.Identity
import System.FilePath





readCommand :: Application Configuration (Maybe Command)
readCommand = do
    

