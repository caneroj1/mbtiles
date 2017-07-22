{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Mbtiles.Types where

import           Control.Monad.Reader
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Database.SQLite.Simple         (Connection, Statement)
import           Database.SQLite.Simple.ToField

data SqlData = SqlData {
    r    :: Statement
  , conn :: Connection
  }

-- | MbtilesT monad that will run actions on an MBTiles file.
newtype MbtilesT m a = MbtilesT {
    unMbtilesT :: ReaderT SqlData m a
  } deriving (Functor, Applicative, Monad)

-- | Type specialization 'MbtilesT' to IO.
type Mbtiles a = MbtilesT IO a

-- | Newtype wrapper around map zoom level.
newtype Zoom = Z Int deriving ToField

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
