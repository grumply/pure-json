{-# LANGUAGE CPP #-}
module Pure.Data.JSON (parse,decode,encode,decodeBS,encodeBS,module Export) where

import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

#ifdef __GHCJS__
import           Pure.Data.JSON.GHCJS as Export hiding (encode,decode)
import qualified Pure.Data.JSON.GHCJS as GHCJS
#else
import           Pure.Data.JSON.GHC   as Export hiding (encode,decode)
import qualified Pure.Data.JSON.GHC   as GHC
#endif

{-# INLINE parse #-}
parse = flip parseMaybe

{-# INLINE decode #-}
decode :: FromJSON a => Txt -> Maybe a
#ifdef __GHCJS__
decode = GHCJS.decode
#else
decode = GHC.decode . fromTxt
#endif

{-# INLINE encode #-}
encode :: ToJSON a => a -> Txt
#ifdef __GHCJS__
encode = GHCJS.encode . toJSON
#else
encode = toTxt . GHC.encode
#endif

{-# INLINE decodeBS #-}
decodeBS :: FromJSON a => BSL.ByteString -> Maybe a
#ifdef __GHCJS__
decodeBS = GHCJS.decode . toTxt
#else
decodeBS = GHC.decode
#endif

{-# INLINE encodeBS #-}
encodeBS :: ToJSON a => a -> BSL.ByteString
#ifdef __GHCJS__
encodeBS = fromTxt . GHCJS.encode . toJSON
#else
encodeBS = GHC.encode
#endif
