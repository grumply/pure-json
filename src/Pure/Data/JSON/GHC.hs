{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Pure.Data.JSON.GHC (module Pure.Data.JSON.GHC, module Aeson, module Export) where

-- from aeson
import qualified Data.Aeson as Aeson
import Data.Aeson as Export hiding (Options,Object)
import qualified Data.Aeson.Types as O (Object)
import Data.Aeson.Types as Export hiding (Options,Object,parse)

-- from aeson-pretty
import Data.Aeson.Encode.Pretty

-- from base
import Control.Exception
import Data.Maybe
import Data.Typeable
import Prelude hiding (lookup)

-- from pure-txt
import Pure.Data.Txt

-- from text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- from unordered-containers
import qualified Data.HashMap.Strict as Map

-- from vector
import qualified Data.Vector as V

pretty :: ToJSON a => a -> Txt
pretty = TL.toStrict . TL.decodeUtf8 . encodePretty

logJSON :: ToJSON a => a -> IO ()
logJSON = putStrLn . fromTxt . pretty

type Obj = O.Object

-- copied from GHCJS JSON for compatability
data JSONException = UnknownKey
  deriving (Show, Typeable)

instance Exception JSONException

-- from GHCJS.JSON
class Lookup k a where
  (!)       :: k -> a -> Value
  lookup    :: k -> a -> Maybe Value

instance Lookup Txt O.Object where
  (!) k v  = fromMaybe (throw UnknownKey) (lookup k v)
  lookup = Map.lookup

instance Lookup Txt Value where
  (!) k v = fromMaybe (throw UnknownKey) (lookup k v)
  lookup k v =
    case v of
      Aeson.Object o -> lookup k o
      _ -> Nothing

instance Lookup Int Aeson.Array where
  (!) i a = fromMaybe (throw UnknownKey) (lookup i a)
  lookup = flip (V.!?)

instance Lookup Int Value where
  (!) i a      = fromMaybe (throw UnknownKey) (lookup i a)
  lookup i v =
    case v of
      Array arr -> lookup i arr
      _ -> Nothing

instance ToTxt Value where
   {-# INLINE toTxt #-}
   toTxt = TL.toStrict . TL.decodeUtf8 . encode

