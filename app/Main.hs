{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import           Database.Mbtiles

main = do
  let myData = "myTileData" :: BL.ByteString
  runMbtiles "my/path/to/file.mbtiles" $ do
    maybeTileData <- getTile (Z 0) (X 0) (Y 0)
    case maybeTileData of
      Nothing  -> writeTile (Z 0) (X 0) (Y 0) myData
      (Just d) -> updateTile (Z 0) (X 0) (Y 0) $ BL.init d
