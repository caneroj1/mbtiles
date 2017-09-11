{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Database.Mbtiles.Types where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import qualified Data.HashMap.Strict              as M
import           Data.Monoid
import           Data.Text                        (Text)
import           Data.Tile
import           Database.SQLite.Simple           (Connection, Statement)
import           Database.SQLite.Simple.FromField
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

-- | Data type that represents an entire row
-- from an MBTiles database.
data DataTile a = DataTile Tile a

instance (Show a) => Show (DataTile a) where
  show (DataTile (Tile (Z z, X x, Y y)) td) =
    "Tile " ++ show z ++ "/" ++ show x ++ "/" ++
    show y ++ " " ++ show td

instance (Eq a) => Eq (DataTile a) where
  (DataTile t1 d1) == (DataTile t2 d2) = t1 == t2 && d1 == d2

instance (Ord a) => Ord (DataTile a) where
  (DataTile t1 d1) `compare` (DataTile t2 d2) = compare t1 t2 <>
                                                compare d2 d2

instance Functor DataTile where
  fmap f (DataTile t d) = DataTile t $ f d

instance (FromTile a) => FromRow (DataTile a) where
  fromRow = DataTile . Tile <$>
              ((,,) <$>
                field <*>
                field <*>
                field) <*>
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

-- * Orphan Instances
instance FromField Z where
  fromField = fmap Z . fromField

instance FromField X where
  fromField = fmap X . fromField

instance FromField Y where
  fromField = fmap Y . fromField

instance ToField Z where
  toField (Z z) = toField z

instance ToField X where
  toField (X x) = toField x

instance ToField Y where
  toField (Y y) = toField y

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
