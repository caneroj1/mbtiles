module Database.Mbtiles.Utility where

import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict    as M
import           Data.List
import           Data.Text              (Text)
import           Database.Mbtiles.Query
import           Database.Mbtiles.Types
import           Database.SQLite.Simple

openStmt :: (MonadIO m) => Connection -> Query -> m Statement
openStmt c = liftIO . openStatement c

closeStmt :: (MonadIO m) => Statement -> m ()
closeStmt = liftIO . closeStatement

openConn :: (MonadIO m) => FilePath -> m Connection
openConn = liftIO . open

closeConn :: (MonadIO m) => Connection -> m ()
closeConn = liftIO . close

doesTableExist :: (MonadIO m) => Connection -> Text -> m Bool
doesTableExist conn tableName =
  checkResults <$> liftIO tableQuery
  where tableQuery :: IO [Only Int]
        tableQuery = query conn tableExistsQuery (Only tableName)
        checkResults []            = False
        checkResults (Only r : ls) = r > 0

getColumnNames :: (MonadIO m) => Connection -> Text -> m [Text]
getColumnNames conn tableName =
  sort . map (snd6 . unCI) <$> liftIO columnInfoQuery
  where columnInfoQuery = query_ conn $ tableInfoQuery tableName

snd6 :: (a, b, c, d, e, f) -> b
snd6 (_, b, _, _, _, _) = b

validator :: (MonadIO m)
          => (Connection -> m (Either MBTilesError a))
          -> Either MBTilesError Connection
          -> m (Either MBTilesError a)
validator = either (return . Left)

columnChecker :: (MonadIO m)
              => Text
              -> [Text]
              -> MBTilesError
              -> Connection
              -> m (Either MBTilesError Connection)
columnChecker tableName cols err conn = do
  cs <- getColumnNames conn tableName
  if cs /= cols then return $ Left err else return $ Right conn

getDBMetadata :: (MonadIO m) => Connection -> m MbtilesMeta
getDBMetadata conn = M.fromList <$> liftIO (query_ conn getMetadataQuery)
