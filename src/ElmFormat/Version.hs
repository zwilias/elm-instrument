module ElmFormat.Version (asString, experimental) where

import qualified Build_elm_instrument


asString :: String
asString =
    Build_elm_instrument.gitDescribe


experimental :: Maybe String
experimental =
    Nothing
