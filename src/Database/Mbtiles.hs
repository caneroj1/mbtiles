{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Mbtiles
(
  MbtilesT
, Mbtiles
, Zoom (..)
, X (..)
, Y (..)
, runMbtilesT
, getTile
) where

import           Control.Exception.Base
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy     as BL
import           Database.Mbtiles.Query
import           Database.Mbtiles.Utility
import           Database.SQLite.Simple

newtype MbtilesT m a = MbtilesT {
    unMbtilesT :: ReaderT Stmts (ExceptT IOException m) a
  } deriving (Functor, Applicative, Monad)

type Mbtiles a = MbtilesT IO a

data Stmts = Stmts {
    r :: Statement
  , w :: Statement
  , u :: Statement
  }

newtype Zoom = Z Int
newtype X = X Int
newtype Y = Y Int

runMbtilesT :: (MonadIO m) => FilePath -> MbtilesT m a -> m (Either IOException a)
runMbtilesT mbtilesPath mbt = do
  c <- liftIO $ open mbtilesPath
  s <- liftIO $ stmts c
  v <- runExceptT $ runReaderT (unMbtilesT mbt) s
  closeAll s >> liftIO (close c)
  return v
  where
    stmts c =
      Stmts <$>
        openStmt c getTileQuery <*>
        openStmt c newTileQuery <*>
        openStmt c updateTileQuery
    closeAll Stmts{r = rs, w = ws, u = us} =
      closeStmt rs >> closeStmt ws >> closeStmt us

getTile :: (MonadIO m) => Zoom -> X -> Y -> MbtilesT m (Maybe (Only BL.ByteString))
getTile (Z z) (X x) (Y y) = MbtilesT $ do
  rs <- r <$> ask
  liftIO $ do
    bindNamed rs [":zoom" := z, ":col" := x, ":row" := y]
    res <- nextRow rs
    reset rs
    return res
