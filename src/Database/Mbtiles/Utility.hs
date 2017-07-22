module Database.Mbtiles.Utility where

import           Control.Monad.IO.Class
import           Database.SQLite.Simple

openStmt :: (MonadIO m) => Connection -> Query -> m Statement
openStmt c = liftIO . openStatement c

closeStmt :: (MonadIO m) => Statement -> m ()
closeStmt = liftIO . closeStatement
