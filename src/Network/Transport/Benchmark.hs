-- |
-- Module: Network.Transport.Benchmark
-- Copyright: (C) 2015, Tweag I/O
--
--
module Network.Transport.Benchmark
  ( benchmarkPingLocal
  , BenchmarkOptions(..)
  ) where


import Control.Applicative
import Network.Transport
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
import Control.Concurrent.Async
import Control.Exception (throw)
import Prelude

data BenchmarkOptions = BenchmarkOptions
  { roundCount  :: Int -- ^ Number of pings to send
  , messageSize :: Int -- ^ Size of chunk in message
  , chunkNum    :: Int -- ^ Number of chunks
  }

-- | Test pings between local endpoints, each endpoint creates connection
-- to each other endpoint and sends @messageSize@ pings over those connections,
-- in a separate thread connection waits for process number * @messageSize@ events
-- to arrive. Messages are not explictily forced by benchmark.
benchmarkPingLocal :: [EndPoint]
                   -> BenchmarkOptions
                   -> IO ()
benchmarkPingLocal eps options = void $ mapConcurrently pingProcess eps where
  names = map address eps
  pingProcess ep = void (join (waitBoth <$> async sender <*> async receiver)) where
    sender = mapM mkConnection names >>= wrapper where
      mkConnection c = either throw id <$> connect ep c ReliableOrdered
                                                        defaultConnectHints
      wrapper connections = go (roundCount options) connections where
        go 0 []     = return ()
        go n []     = go (n-1) connections
        go n (c:cs) = do either throw id <$> send c (mkMessage options)
                         go n cs
    receiver = go (length eps * roundCount options) where
      go 0 = return ()
      go n = do
        ev <- receive ep
        case ev of
          Received _ _ -> go (n-1)
          _            -> go n

mkMessage :: BenchmarkOptions -> [ByteString]
mkMessage (BenchmarkOptions _ n c) = go n where
  go 0 = []
  go n = BS.replicate c 42:go (n-1)
