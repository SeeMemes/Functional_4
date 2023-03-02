module Text.SimpleJSON.DeriveVia where
import Data.Coerce
import Data.Data

import Text.SimpleJSON.Generic ( JSON(readJSON, showJSON), toJSONGeneric, fromJSONGeneric )
import Text.SimpleJSON.Result ( Result )
import Text.SimpleJSON.Types ( JSValue )

newtype CustomJSON a = CustomJSON { unCustomJSON :: a }

instance (Data a) => JSON (CustomJSON a) where
  readJSON :: Data a => JSValue -> Result (CustomJSON a)
  readJSON = (coerce `asTypeOf` fmap CustomJSON) . fromJSONGeneric
  showJSON :: Data a => CustomJSON a -> JSValue
  showJSON = toJSONGeneric . unCustomJSON
