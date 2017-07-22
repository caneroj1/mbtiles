{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Mbtiles.Types where

import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Text                      (Text)
import           Database.SQLite.Simple         (Connection, Statement)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToField

data SqlData = SqlData {
    r    :: Statement
  , conn :: Connection
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
    unMbtilesT :: ReaderT SqlData m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

-- | Type specialization 'MbtilesT' to IO.
type Mbtiles a = MbtilesT IO a

-- | Newtype wrapper around map zoom level.
newtype Z = Z Int deriving ToField

-- | Newtype wrapper around a tile's x-coordinate.
newtype X = X Int deriving ToField

-- | Newtype wrapper around a tile's y-coordinate.
newtype Y = Y Int deriving ToField

-- | Typeclass representing data types that can be turned
-- into a lazy ByteString and stored as tile data.
class ToTile a where
  toTile :: a -> BL.ByteString

instance ToTile BS.ByteString where
  toTile = BL.fromStrict

instance ToTile BL.ByteString where
  toTile = id

-- | Typeclass representing data types intp which raw tile data can
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

metadataColumns, tilesColumns :: [Text]
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

