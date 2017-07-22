# mbtiles

Haskell library for interfacing with MapBox [MBTiles](https://github.com/mapbox/mbtiles-spec) files.

## Functionality
* Getting tiles by zoom, x, and y.
* Writing new tiles by zoom, x, and y.
* Updating existing tiles by zoom, x, and y.

## Basic Usage

```haskell
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


```

## Future Work
* Improve database error handling.
* Investigate usage as a performant tile server.
* Add tests.