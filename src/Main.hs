{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Exception (bracket, tryJust)
import Control.Monad
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Digest.XXHash (xxHash')
import Data.Monoid ((<>))
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Directory (listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath ((</>), takeFileName)
import System.IO (FilePath)
import System.IO.Error (isDoesNotExistError)
import System.IO.MMap (mmapFileByteString)
import System.Posix (getFileStatus, isDirectory, isRegularFile)

-- Primary key, Absolute path to file, filename, xxhash
data FileField = FileField Int FilePath String Int
    deriving (Show)

instance FromRow FileField where
    fromRow = FileField <$> field <*> field <*> field <*> field

data Stuff = Stuff
    { ignoreThese :: [String]
    , dbConn :: Connection
    }

type Indexer m = ReaderT Stuff m

hashIt :: (MonadIO m) => FilePath -> Indexer m ()
hashIt p = do
    hash <- liftIO ((show . xxHash') <$> mmapFileByteString p Nothing)
    conn <- asks dbConn
    liftIO $ putStrLn $ "File: " <> p <> " HASH: " <> hash
    liftIO $ execute conn "INSERT INTO file_index (path, fn, hash) VALUES (?,?,?)" (p, takeFileName p, hash)

runner :: (MonadIO m) => FilePath -> Indexer m ()
runner path = do
    ignorePatterns <- asks ignoreThese
    ps <- liftIO (filter (`notElem` ignorePatterns) <$> listDirectory path)
    forM_ ps $ \f -> do
        f' <- liftIO $ makeAbsolute $ path </> f
        fs <- liftIO $ tryJust (guard . isDoesNotExistError) (getFileStatus f')
        case fs of
          Left _    -> liftIO $ putStrLn ("File does not exist: " <> f')
          Right fs' -> dispatcher f' fs'
  where
      dispatcher fn fStatus
        | isRegularFile fStatus = hashIt fn
        | isDirectory   fStatus = runner fn
        | otherwise             = liftIO $ putStrLn "This file isn't anything, strange eh?"

-- TODO:
--   * Add hostname to the file table.
--   * Add one table for hashes, one table for files and one table to connect them.

main :: IO ()
main = do
    args <- getArgs
    bracket (open "files.sqlite3") close $ \conn -> do
        execute_ conn "CREATE TABLE IF NOT EXISTS file_index (id INTEGER PRIMARY KEY, path TEXT, fn TEXT, hash INTEGER)"
        runReaderT (runner (args !! 0)) (Stuff [".git"] conn)
