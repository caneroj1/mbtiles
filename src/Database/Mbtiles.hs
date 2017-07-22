{-# LANGUAGE OverloadedStrings #-}

module Database.Mbtiles
(
  MbtilesT
, Mbtiles
, Zoom(..)
, X(..)
, Y(..)
, runMbtilesT
, getTile
, writeTile
, writeTiles
, updateTile
, updateTiles
) where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as BL
import           Database.Mbtiles.Query
import           Database.Mbtiles.Types
import           Database.Mbtiles.Utility
import           Database.SQLite.Simple

-- | Given a path to an MBTiles file, run the 'MbtilesT' action.
-- This will open a connection the MBTiles file, run the action,
-- and then close the connection.
runMbtilesT :: (MonadIO m) => FilePath -> MbtilesT m a -> m a
runMbtilesT mbtilesPath mbt = do
  c <- openConn mbtilesPath
  s <- mkSqlData c
  v <- runReaderT (unMbtilesT mbt) s
  closeAll s
  return v
  where
    mkSqlData c =
      SqlData <$>
        openStmt c getTileQuery <*>
        pure c
    closeAll SqlData{r = rs, conn = c} =
      closeStmt rs >> closeConn c

-- | Specialized version of 'runMbtilesT' to run in the IO monad.
runMbtiles :: FilePath -> Mbtiles a -> IO a
runMbtiles mbtilesPath mbt = runMbtilesT

-- | Given a 'Zoom', 'X', and 'Y' parameters, return the corresponding tile data,
-- if it exists.
getTile :: (MonadIO m, FromTile a) => Zoom -> X -> Y -> MbtilesT m (Maybe a)
getTile (Z z) (X x) (Y y) = MbtilesT $ do
  rs <- r <$> ask
  fmap unwrapTile <$> liftIO (do
    bindNamed rs [":zoom" := z, ":col" := x, ":row" := y]
    res <- nextRow rs
    reset rs
    return res)
  where unwrapTile (Only bs) = fromTile bs

-- | Write new tile data to the tile at the specified 'Zoom', 'X', and 'Y' parameters.
-- This function assumes that the tile does not already exist.
writeTile :: (MonadIO m, ToTile a) => Zoom -> X -> Y -> a -> MbtilesT m ()
writeTile z x y t = writeTiles [(z, x, y, t)]

-- | Batch write new tile data to the tile at the specified 'Zoom', 'X', and 'Y' parameters.
-- This function assumes that the tiles do not already exist.
writeTiles :: (MonadIO m, ToTile a) => [(Zoom, X, Y, a)] -> MbtilesT m ()
writeTiles = execQueryOnTiles newTileQuery

-- | Update existing tile data for the tile at the specified 'Zoom', 'X', and 'Y' parameters.
-- This function assumes that the tile does already exist.
updateTile :: (MonadIO m, ToTile a) => Zoom -> X -> Y -> a -> MbtilesT m ()
updateTile z x y t = updateTiles [(z, x, y, t)]

-- | Batch update tile data for the tiles at the specified 'Zoom', 'X', and 'Y' parameters.
-- This function assumes that the tiles do already exist.
updateTiles :: (MonadIO m, ToTile a) => [(Zoom, X, Y, a)] -> MbtilesT m ()
updateTiles = execQueryOnTiles updateTileQuery

execQueryOnTiles :: (MonadIO m, ToTile a) => Query -> [(Zoom, X, Y, a)] -> MbtilesT m ()
execQueryOnTiles q ts = MbtilesT $ do
  c <- conn <$> ask
  liftIO $
    executeMany c q $
      map (\(z, x, y, t) -> (z, z, y, toTile t)) ts
