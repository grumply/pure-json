{-# LANGUAGE CPP #-}
module Pure.Data.JSON (parse,decode,decodeEither,encode,decodeBS,decodeBSEither,encodeBS,object,traceJSON,module Export) where

import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

#ifdef __GHCJS__
import           Pure.Data.JSON.GHCJS as Export hiding (encode,decode,decodeEither,object)
import qualified Pure.Data.JSON.GHCJS as GHCJS
#else
import           Pure.Data.JSON.GHC   as Export hiding (encode,decode,object)
import qualified Pure.Data.JSON.GHC   as GHC
#endif

import System.IO.Unsafe (unsafePerformIO)

{-# INLINE parse #-}
parse = flip parseMaybe

{-# INLINE decode #-}
decode :: FromJSON a => Txt -> Maybe a
#ifdef __GHCJS__
decode = GHCJS.decode
#else
decode = GHC.decode . fromTxt
#endif

{-# INLINE decodeEither #-}
decodeEither :: FromJSON a => Txt -> Either String a
#ifdef __GHCJS__
decodeEither = GHCJS.decodeEither
#else
decodeEither = GHC.eitherDecode . fromTxt
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

{-# INLINE decodeBSEither #-}
decodeBSEither :: FromJSON a => BSL.ByteString -> Either String a
#ifdef __GHCJS__
decodeBSEither = GHCJS.decodeEither . toTxt
#else
decodeBSEither = GHC.eitherDecode
#endif

{-# INLINE encodeBS #-}
encodeBS :: ToJSON a => a -> BSL.ByteString
#ifdef __GHCJS__
encodeBS = fromTxt . GHCJS.encode . toJSON
#else
encodeBS = GHC.encode
#endif

{-# INLINE object #-}
object :: [Pair] -> Value
#ifdef __GHCJS__
object = GHCJS.objectValue . GHCJS.object
#else
object = GHC.object
#endif

traceJSON :: ToJSON a => a -> b -> b
traceJSON a b = 
  let 
    x = unsafePerformIO (logJSON a)
  in 
    x `seq` b