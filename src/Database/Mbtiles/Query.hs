{-# LANGUAGE OverloadedStrings #-}

module Database.Mbtiles.Query where

import           Database.SQLite.Simple

getTileQuery :: Query
getTileQuery = " select tile_data from tiles \
               \ where zoom_level  = :zoom   \
               \ and   tile_column = :col    \
               \ and   tile_row    = :row"

updateTileQuery :: Query
updateTileQuery = " update tiles           \
                  \ set tile_data = :?     \
                  \ where zoom_level  = :? \
                  \ and   tile_column = :? \
                  \ and   tile_row    = :?"

newTileQuery :: Query
newTileQuery = " insert into tiles                                         \
               \ (zoom_level, tile_column, tile_row, tile_data)            \
               \ values (?, ?, ?, ?) "
