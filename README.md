# mbtiles

Haskell library for interfacing with MapBox [MBTiles](https://github.com/mapbox/mbtiles-spec) files.

Documentation available on [Hackage](https://hackage.haskell.org/package/mbtiles).

## Functionality
* Getting tiles by zoom, x, and y.
* Writing new tiles by zoom, x, and y.
* Updating existing tiles by zoom, x, and y.
* Accessing metadata from the mbtiles file.

## Basic Usage

Reading, writing, and updating tiles:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import           Database.Mbtiles

main = do
  let myData = "myTileData" :: BL.ByteString
  let tile   = Tile (Z 0, X 0, Y 0)
  runMbtiles "my/path/to/file.mbtiles" $ do
    maybeTileData <- getTile tile
    case maybeTileData of
      Nothing  -> writeTile tile myData
      (Just d) -> updateTile tile $ BL.init d
```

Getting metadata:

```haskell

import Control.Monad.IO.Class
import Database.Mbtiles

main = do
  runMbtiles "my/path/to/file.mbtiles" $ do
    liftIO . print =<< getName
    liftIO . print =<< getType
    liftIO . print =<< getFormat

```

## Future Work
* Improve database error handling.
* Investigate usage as a performant tile server.
* Add tests.