{-# LANGUAGE CPP #-}
module Pure.Data.JSON (parse,module Export) where

#ifdef __GHCJS__
import Pure.Data.JSON.GHCJS as Export
#else
import Pure.Data.JSON.GHC   as Export
#endif

parse = flip parseMaybe
