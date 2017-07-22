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

newtype MbtilesT m a = MbtilesT {
    unMbtilesT :: ReaderT SqlData m a
  } deriving (Functor, Applicative, Monad)

type Mbtiles a = MbtilesT IO a

newtype Zoom = Z Int deriving ToField
newtype X = X Int deriving ToField
newtype Y = Y Int deriving ToField

class ToTile a where
  toTile :: a -> BL.ByteString

instance ToTile BS.ByteString where
  toTile = BL.fromStrict

instance ToTile BL.ByteString where
  toTile = id

class FromTile a where
  fromTile :: BL.ByteString -> a

instance FromTile BS.ByteString where
  fromTile = BL.toStrict

instance FromTile BL.ByteString where
  fromTile = id
