{-# LANGUAGE OverloadedStrings #-}

module Database.Mbtiles
(
  MbtilesT
, Mbtiles
, MBTilesError(..)
, Z(..)
, X(..)
, Y(..)
, runMbtilesT
, runMbtiles
, getTile
, writeTile
, writeTiles
, updateTile
, updateTiles
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as BL
import           Data.Monoid
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
    mkSqlData c =
      SqlData <$>
        openStmt c getTileQuery <*>
        pure c
    closeAll SqlData{r = rs, conn = c} =
      closeStmt rs >> closeConn c
    processMbt c = do
      s <- mkSqlData c
      v <- runReaderT (unMbtilesT mbt) s
      closeAll s
      return $ Right v

validateMBTiles :: (MonadIO m) => FilePath -> m (Either MBTilesError Connection)
validateMBTiles mbtilesPath = liftIO $
  doesFileExist mbtilesPath >>=
  ifExistsOpen              >>=
  validator schema          >>=
  validator metadata        >>=
  validator tiles
  where
    ifExistsOpen False = return $ Left DoesNotExist
    ifExistsOpen True  = Right <$> open mbtilesPath

    schema c = do
      valid <- mconcat $ map (fmap All) [doesTableExist c tilesTable, doesTableExist c metadataTable]
      if getAll valid then return $ Right c else return $ Left InvalidSchema

    metadata = columnChecker metadataTable metadataColumns InvalidMetadata
    tiles = columnChecker tilesTable tilesColumns InvalidTiles

-- | Specialized version of 'runMbtilesT' to run in the IO monad.
runMbtiles :: FilePath -> Mbtiles a -> IO (Either MBTilesError a)
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
