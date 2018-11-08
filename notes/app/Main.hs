{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Note
import SQLiteProvider
import DataProvider
import Data.Time.Clock
import Data.Time.Calendar


main :: IO()
main = do
    let p = mkSQLiteProvider "test.sqlite"

    b <- exists p
    when (not b) (create p)
    
    time <- getCurrentTime
    
    let n = Note noId Normal "meow meow" time

    deleteNote p 1
    --saveNote p n
