{-# LANGUAGE OverloadedStrings #-}

module Thrift.Protocol.Multiplexed
       ( module Thrift.Protocol
       , MultiplexedProtocol(..)
       ) where

import Thrift.Protocol
import Thrift.Types

import qualified Data.Text.Lazy as LT

-- | The Multiplexed Protocol data uses the standard 'TSimpleJSONProtocol'.  Data is
-- encoded using the underlying protocol.
data MultiplexedProtocol p t =
  MultiplexedProtocol (p t) LT.Text
  -- ^ Construct a 'MultiplexedProtocol' with an underlying 'Protocol'

instance Protocol p => Protocol (MultiplexedProtocol p) where
  getTransport (MultiplexedProtocol p _) = getTransport p
  readMessageBegin (MultiplexedProtocol p _) = readMessageBegin p
  serializeVal (MultiplexedProtocol p _) = serializeVal p
  deserializeVal (MultiplexedProtocol p _) = deserializeVal p
  readVal (MultiplexedProtocol p _) = readVal p
  writeMessageBegin (MultiplexedProtocol p serviceName) m@(s, ty, sq) =
    writeMessageBegin p m'
    where
      m' = case ty of
        M_REPLY     -> m
        M_EXCEPTION -> m
        M_CALL      -> (s', ty, sq)
        M_ONEWAY    -> (s', ty, sq)
      s' = LT.concat [serviceName, sEPARATOR, s]

sEPARATOR :: LT.Text
sEPARATOR = ":"
