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
validateMetadata = undefined
