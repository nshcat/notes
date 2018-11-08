module Note
    ( Note(..)
    , Priority(..)
    , noId
    ) where
    
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text

data Priority = High | Normal | Low
    deriving (Show, Read, Eq)
    
data Note = Note
    { nid       :: Int
    , priority  :: Priority
    , content   :: Text
    , dueDate   :: UTCTime
    } deriving (Show, Read)

noId :: Int
noId = -1
