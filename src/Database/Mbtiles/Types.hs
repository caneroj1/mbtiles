{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Mbtiles.Types where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.HashMap.Strict            as M
import           Data.Monoid
import           Data.Text                      (Text)
import           Database.SQLite.Simple         (Connection, Statement)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField

-- | MBTiles files contain metadata in one of their tables. This is
-- a type alias for a mapping between the metadata key and the metadata value.
type MbtilesMeta = M.HashMap Text Text

data MbtilesData = MbtilesData {
    r    :: !Statement
  , conn :: !Connection
  , meta :: !MbtilesMeta
  }

-- | Data type representing various errors that could occur
-- when opening and validating an MBTiles file.
data MBTilesError = DoesNotExist    -- ^ The MBTiles file does not exist.
                  | InvalidSchema   -- ^ The MBTiles schema is invalid according to the spec.
                  | InvalidMetadata -- ^ The MBTiles 'metadata' table is invalid.
                  | InvalidTiles    -- ^ The MBTiles 'tiles' table is invaid.
                  deriving (Show, Eq)

-- | MbtilesT monad that will run actions on an MBTiles file.
newtype MbtilesT m a = MbtilesT {
    unMbtilesT :: ReaderT MbtilesData m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

instance (MonadIO m) => MonadIO (MbtilesT m) where
  liftIO = MbtilesT . liftIO

-- | Type specialization of 'MbtilesT' to IO.
type MbtilesIO a = MbtilesT IO a

-- | Newtype wrapper around map zoom level.
newtype Z = Z Int deriving ToField

-- | Newtype wrapper around a tile's x-coordinate.
newtype X = X Int deriving ToField

-- | Newtype wrapper around a tile's y-coordinate.
newtype Y = Y Int deriving ToField

-- | Data type that represents an entire row
-- from an MBTiles database.
data Tile a = Tile {
    tileColumn :: Int -- ^ The column of this tile.
  , tileRow    :: Int -- ^ The row of this tile.
  , zoomlevel  :: Int -- ^ The zoom level of this tile.
  , tileData   :: a   -- ^ The data associated with this tile.
  }

instance (Show a) => Show (Tile a) where
  show (Tile tc tr zl td) = show zl ++ "/" ++
                            show tc ++ "/" ++
                            show tr ++ " " ++
                            show td

instance (Eq a) => Eq (Tile a) where
  (Tile c1 r1 z1 d1) == (Tile c2 r2 z2 d2) = c1 == c2 &&
                                             r1 == r2 &&
                                             z1 == z2 &&
                                             d1 == d2

instance (Ord a) => Ord (Tile a) where
  (Tile c1 r1 z1 d1) `compare` (Tile c2 r2 z2 d2) = compare z1 z2 <>
                                                    compare c1 c2 <>
                                                    compare r1 r2 <>
                                                    compare d1 d2

instance Functor Tile where
  fmap f (Tile c r z d) = Tile c r z $ f d

instance (FromTile a) => FromRow (Tile a) where
  fromRow = Tile <$>
              field <*>
              field <*>
              field <*>
              (fromTile <$> field)

-- | A 'TileStream' data type contains information about how to
-- stream tiles from the MBTiles database and is used in the same
-- manner as an SQLite prepared statement.
newtype TileStream = TileStream Statement

-- | Typeclass representing data types that can be turned
-- into a lazy ByteString and stored as tile data.
class ToTile a where
  toTile :: a -> BL.ByteString

instance ToTile BS.ByteString where
  toTile = BL.fromStrict

instance ToTile BL.ByteString where
  toTile = id

-- | Typeclass representing data types into which raw tile data can
-- be converted.
class FromTile a where
  fromTile :: BL.ByteString -> a

instance FromTile BS.ByteString where
  fromTile = BL.toStrict

instance FromTile BL.ByteString where
  fromTile = id

newtype ColumnInfo = ColumnInfo {
    unCI :: (Int, Text, Text, Bool, Maybe Int, Int)
  }

instance FromRow ColumnInfo where
  fromRow = ColumnInfo <$> fromRow

metadataTable, tilesTable :: Text
metadataTable = "metadata"
tilesTable = "tiles"

metadataColumns, tilesColumns, requiredMeta :: [Text]
metadataColumns = [
    "name"
  , "value"
  ]

tilesColumns = [
    "tile_column"
  , "tile_data"
  , "tile_row"
  , "zoom_level"
  ]

requiredMeta = [
    "name"
  , "type"
  , "version"
  , "description"
  , "format"
  ]
