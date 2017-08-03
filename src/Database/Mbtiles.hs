{-|
Module      : Database.Mbtiles
Description : Haskell MBTiles client.
Copyright   : (c) Joe Canero, 2017
License     : BSD3
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX

This module provides support for reading, writing, and updating
an mbtiles database. There is also functionality for reading
metadata from the database.

There is also functionality for creating a pool of connections to
an mbtiles database.

See the associated README.md for basic usage examples.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Database.Mbtiles
(
  -- * Types
  MbtilesT
, MbtilesIO
, MbtilesMeta
, MBTilesError(..)
, Z(..)
, X(..)
, Y(..)

  -- * Typeclasses
, ToTile(..)
, FromTile(..)

  -- * The MbtilesT monad transformer
, runMbtilesT
, runMbtiles

  -- ** Pooling
, MbtilesPool
, getMbtilesPool
, runMbtilesPoolT

  -- * Mbtiles read/write functionality
, getTile
, writeTile
, writeTiles
, updateTile
, updateTiles

  -- * Mbtiles metadata functionality
, getMetadata
, getName
, getType
, getVersion
, getDescription
, getFormat
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy        as BL
import           Data.HashMap.Strict         ((!))
import qualified Data.HashMap.Strict         as M hiding ((!))
import           Data.Monoid
import           Data.Pool
import           Data.Text                   (Text)
import           Database.Mbtiles.Query
import           Database.Mbtiles.Types
import           Database.Mbtiles.Utility
import           Database.SQLite.Simple
import           System.Directory

-- | Given a path to an MBTiles file, run the 'MbtilesT' action.
-- This will open a connection to the MBTiles file, run the action,
-- and then close the connection.
-- Some validation will be performed first. Of course, we will check if the
-- MBTiles file actually exists. If it does, we need to validate its schema according
-- to the MBTiles spec.
runMbtilesT :: (MonadIO m) => FilePath -> MbtilesT m a -> m (Either MBTilesError a)
runMbtilesT mbtilesPath mbt = do
  m <- validateMBTiles mbtilesPath
  either (return . Left) processMbt m
  where
    processMbt (c, d) = do
      m <- mkMbtilesData c d
      v <- runReaderT (unMbtilesT mbt) m
      closeAll m
      return $ Right v

-- | A pool of connections to an MBTiles database.
type MbtilesPool = Pool MbtilesData

-- | Given a path to an MBTiles file, create a connection pool
-- to an MBTiles database. This will perform the same validation as 'runMbtilesT'.
getMbtilesPool :: (MonadIO m) => FilePath -> m (Either MBTilesError MbtilesPool)
getMbtilesPool fp = do
  m <- validateMBTiles fp
  either (return . Left) (fmap Right . liftIO . buildPool) m
  where
    buildPool (_, d) =
      createPool (openConnection d) closeAll 1 900 1000
    openConnection d = open fp >>= flip mkMbtilesData d

-- | Given access to an 'MbtilesPool', run an action against that pool.
runMbtilesPoolT :: (MonadBaseControl IO m) => MbtilesPool -> MbtilesT m a -> m a
runMbtilesPoolT p mbt = withResource p (runReaderT (unMbtilesT mbt))

type ValidationResult = (Connection, MbtilesMeta)

closeAll :: (MonadIO m) => MbtilesData -> m ()
closeAll MbtilesData{r = rs, conn = c} =
      closeStmt rs >> closeConn c

mkMbtilesData :: (MonadIO m) => Connection -> MbtilesMeta -> m MbtilesData
mkMbtilesData c d =
      MbtilesData <$>
        openStmt c getTileQuery <*>
        pure c                  <*>
        pure d

validateMBTiles :: (MonadIO m) => FilePath -> m (Either MBTilesError ValidationResult)
validateMBTiles mbtilesPath = liftIO $
  doesFileExist mbtilesPath >>=
  ifExistsOpen              >>=
  validator schema          >>=
  validator metadata        >>=
  validator tiles           >>=
  validator metadataValues
  where
    ifExistsOpen False = return $ Left DoesNotExist
    ifExistsOpen True  = Right <$> open mbtilesPath

    schema c = do
      valid <- mconcat $ map (fmap All) [doesTableExist c tilesTable, doesTableExist c metadataTable]
      if getAll valid then return $ Right c else return $ Left InvalidSchema

    metadata = columnChecker metadataTable metadataColumns InvalidMetadata
    tiles = columnChecker tilesTable tilesColumns InvalidTiles
    metadataValues c = do
      m <- getDBMetadata c
      if all (`M.member` m) requiredMeta
        then return $ Right (c, m)
        else return $ Left InvalidMetadata

-- | Specialized version of 'runMbtilesT' to run in the IO monad.
runMbtiles :: FilePath -> MbtilesIO a -> IO (Either MBTilesError a)
runMbtiles = runMbtilesT

-- | Given a 'Z', 'X', and 'Y' parameters, return the corresponding tile data,
-- if it exists.
getTile :: (MonadIO m, FromTile a) => Z -> X -> Y -> MbtilesT m (Maybe a)
getTile (Z z) (X x) (Y y) = MbtilesT $ do
  rs <- r <$> ask
  fmap unwrapTile <$> liftIO (do
    bindNamed rs [":zoom" := z, ":col" := x, ":row" := y]
    res <- nextRow rs
    reset rs
    return res)
  where unwrapTile (Only bs) = fromTile bs

-- | Returns the 'MbtilesMeta' that was found in the MBTiles file.
-- This returns all of the currently available metadata for the MBTiles database.
getMetadata :: (MonadIO m) => MbtilesT m MbtilesMeta
getMetadata = MbtilesT $ reader meta

-- | Helper function for getting the specified name of the MBTiles from metadata.
getName :: (MonadIO m) => MbtilesT m Text
getName = findMeta "name" <$> getMetadata

-- | Helper function for getting the type of the MBTiles from metadata.
getType :: (MonadIO m) => MbtilesT m Text
getType = findMeta "type" <$> getMetadata

-- | Helper function for getting the version of the MBTiles from metadata.
getVersion :: (MonadIO m) => MbtilesT m Text
getVersion = findMeta "version" <$> getMetadata

-- | Helper function for getting the description of the MBTiles from metadata.
getDescription :: (MonadIO m) => MbtilesT m Text
getDescription = findMeta "description" <$> getMetadata

-- | Helper function for getting the format of the MBTiles from metadata.
getFormat :: (MonadIO m) => MbtilesT m Text
getFormat = findMeta "format" <$> getMetadata

-- | Write new tile data to the tile at the specified 'Z', 'X', and 'Y' parameters.
-- This function assumes that the tile does not already exist.
writeTile :: (MonadIO m, ToTile a) => Z -> X -> Y -> a -> MbtilesT m ()
writeTile z x y t = writeTiles [(z, x, y, t)]

-- | Batch write new tile data to the tile at the specified 'Z', 'X', and 'Y' parameters.
-- This function assumes that the tiles do not already exist.
writeTiles :: (MonadIO m, ToTile a) => [(Z, X, Y, a)] -> MbtilesT m ()
writeTiles = execQueryOnTiles newTileQuery

-- | Update existing tile data for the tile at the specified 'Z', 'X', and 'Y' parameters.
-- This function assumes that the tile does already exist.
updateTile :: (MonadIO m, ToTile a) => Z -> X -> Y -> a -> MbtilesT m ()
updateTile z x y t = updateTiles [(z, x, y, t)]

-- | Batch update tile data for the tiles at the specified 'Z', 'X', and 'Y' parameters.
-- This function assumes that the tiles do already exist.
updateTiles :: (MonadIO m, ToTile a) => [(Z, X, Y, a)] -> MbtilesT m ()
updateTiles = execQueryOnTiles updateTileQuery

execQueryOnTiles :: (MonadIO m, ToTile a) => Query -> [(Z, X, Y, a)] -> MbtilesT m ()
execQueryOnTiles q ts = MbtilesT $ do
  c <- conn <$> ask
  liftIO $
    executeMany c q $
      map (\(z, x, y, t) -> (z, z, y, toTile t)) ts

findMeta :: Text -> MbtilesMeta -> Text
findMeta t m = m ! t
