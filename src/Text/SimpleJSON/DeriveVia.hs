module Text.SimpleJSON.DeriveVia where
import Data.Coerce
import Data.Data
import Data.Generics

import Text.SimpleJSON
import Text.SimpleJSON.Generic

newtype CustomJSON a = CustomJSON { unCustomJSON :: a }

instance (Data a) => JSON (CustomJSON a) where
  readJSON = (coerce `asTypeOf` fmap CustomJSON) . fromJSONGeneric
  showJSON = toJSONGeneric . unCustomJSON
