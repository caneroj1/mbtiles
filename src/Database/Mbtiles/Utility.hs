module Database.Mbtiles.Utility where

import           Control.Monad.IO.Class
import           Database.Mbtiles.Query
import           Database.SQLite.Simple

openStmt :: (MonadIO m) => Connection -> Query -> m Statement
openStmt c = liftIO . openStatement c

closeStmt :: (MonadIO m) => Statement -> m ()
closeStmt = liftIO . closeStatement

openConn :: (MonadIO m) => FilePath -> m Connection
openConn = liftIO . open

closeConn :: (MonadIO m) => Connection -> m ()
closeConn = liftIO . close

doesTableExist :: (MonadIO m) => Connection -> String -> m Bool
doesTableExist conn tableName =
  not . null <$> liftIO tableQuery
  where tableQuery :: IO [Only Int]
        tableQuery = query conn tableExistsQuery (Only tableName)
