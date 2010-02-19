{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Binary.Indexed where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy

class HBinary phi h where
  hput :: phi ix -> h ix -> Put
  hget :: phi ix -> Get (h ix)

hdecode :: HBinary phi h => phi ix -> ByteString -> h ix
hdecode phi = runGet (hget phi)

hencode :: HBinary phi h => phi ix -> h ix -> ByteString
hencode phi = runPut . hput phi


