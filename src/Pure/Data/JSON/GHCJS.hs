{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Pure.Data.JSON.GHCJS (module Pure.Data.JSON.GHCJS, module Export) where

import Pure.Data.Txt

import Data.Monoid
import Data.Typeable

import JavaScript.JSON.Types.Instances as Export
import JavaScript.JSON.Types as Export hiding (Options,Object,parse)
import JavaScript.JSON.Types.Internal as Export hiding (Options,Object,parse)
import JavaScript.JSON.Types.Generic as Export

import GHCJS.Types (JSVal)
import GHCJS.Marshal
import GHCJS.Marshal.Pure

import qualified JavaScript.JSON.Types as O (Object)

import Unsafe.Coerce

foreign import javascript unsafe
  "JSON.parse($1)" js_JSON_parse :: Txt -> Value

type Obj = O.Object

instance ToJSVal Obj where
  toJSVal = return . unsafeCoerce

instance PToJSVal Obj where
  pToJSVal = unsafeCoerce

instance FromJSVal Obj where
  fromJSVal = return . Just . unsafeCoerce

instance PFromJSVal Obj where
  pFromJSVal = unsafeCoerce

instance FromJSON Obj where
  parseJSON = withObject "object" pure

instance Eq Value where
  (==) a b = encode a == encode b

instance Show Value where
  show = show . encode

instance ToJSON Obj where
  toJSON = objectValue

foreign import javascript unsafe
  "for (var x in $2) { $1[x] = $2[x]; }" merge_objects_js :: Obj -> Obj -> Obj
  -- shallow, should conform to HashMap (<>)

instance Monoid Obj where
  mempty = emptyObject
  mappend = merge_objects_js

instance ToTxt Value where
  {-# INLINE toTxt #-}
  toTxt = encode

foreign import javascript unsafe
  "JSON.stringify($1,null,4)" pretty_js :: Value -> Txt

pretty :: ToJSON a => a -> Txt
pretty = pretty_js . toJSON

