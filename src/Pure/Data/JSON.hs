{-# LANGUAGE CPP #-}
module Pure.Data.JSON (parse,decode,module Export) where

import Pure.Data.Txt (fromTxt,Txt)

#ifdef __GHCJS__
import           Pure.Data.JSON.GHCJS as Export hiding (decode)
import qualified Pure.Data.JSON.GHCJS as GHCJS
#else
import           Pure.Data.JSON.GHC   as Export hiding (decode)
import qualified Pure.Data.JSON.GHC   as GHC
#endif

parse = flip parseMaybe

decode :: FromJSON a => Txt -> Maybe a
#ifdef __GHCJS__
decode = GHCJS.decode
#else
decode = GHC.decode . fromTxt
#endif
