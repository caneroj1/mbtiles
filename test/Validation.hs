module Validation where

import           Database.Mbtiles
import           Test.HUnit.Base

-- sanity check to make sure we realize when a file doesn't exist.
validateFileDetection :: Assertion
validateFileDetection = do
  e <- runMbtiles "./non-existent-file.mbtiles" (return ())
  Left DoesNotExist @=? e

-- invalid.mbtiles is missing the "metadata" table.
validateSchema :: Assertion
validateSchema = do
  e <- runMbtiles "./mbtiles/invalid.mbtiles" (return ())
  Left InvalidSchema @=? e

validateMetadata :: Assertion
validateMetadata = do
  e <- runMbtiles "./mbtiles/invalid_metadata.mbtiles" (return ())
  Left InvalidMetadata @=? e

validateTiles :: Assertion
validateTiles = do
  e <- runMbtiles "./mbtiles/invalid_tiles.mbtiles" (return ())
  Left InvalidTiles @=? e

validateMetadataValues :: Assertion
validateMetadataValues = do
  e <- runMbtiles "./mbtiles/missing_metadata.mbtiles" (return ())
  Left InvalidMetadata @=? e
