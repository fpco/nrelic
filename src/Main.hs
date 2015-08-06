{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as S
import           Data.Void
import           GHC.Stats
import           Network.Helics
import           System.Environment
import           System.IO.Error

main :: IO ()
main =
  do key:_ <- getArgs
     withHelics
       (def {appName = "GHC Stats Example"
            ,licenseKey = S.pack key})
       (do forkIO (vacuous recordGhcMetrics)
           vacuous makeGarbage)

makeGarbage :: forall (m :: * -> *).
               (Monad m,MonadIO m)
            => m Void
makeGarbage =
  do liftIO (readFile "/proc/meminfo")
     liftIO (threadDelay (1000 * 1000))
     makeGarbage

recordGhcMetrics :: forall (m :: * -> *).
                    (Monad m,MonadIO m)
                 => m Void
recordGhcMetrics =
  do recordStats =<< liftIO getGCStats
     liftIO (threadDelay (10 * 1000 * 1000))
     recordGhcMetrics
  where recordStats :: forall (m :: * -> *).
                       (Monad m,MonadIO m)
                    => GCStats -> m ()
        recordStats GCStats{..} =
          do record "GCStats/bytesAllocated" bytesAllocated
             record "GCStats/bytesCopied" bytesCopied
             record "GCStats/cpuSeconds" cpuSeconds
             record "GCStats/cumulativeBytesUsed" cumulativeBytesUsed
             record "GCStats/currentBytesSlop" currentBytesSlop
             record "GCStats/currentBytesUsed" currentBytesUsed
             record "GCStats/gcCpuSeconds" gcCpuSeconds
             record "GCStats/gcWallSeconds" gcWallSeconds
             record "GCStats/maxBytesSlop" maxBytesSlop
             record "GCStats/maxBytesUsed" maxBytesUsed
             record "GCStats/mutatorCpuSeconds" mutatorCpuSeconds
             record "GCStats/mutatorWallSeconds" mutatorWallSeconds
             record "GCStats/numByteUsageSamples" numByteUsageSamples
             record "GCStats/numGcs" numGcs
             record "GCStats/parMaxBytesCopied" parMaxBytesCopied
             record "GCStats/parTotBytesCopied" parTotBytesCopied
             record "GCStats/peakMegabytesAllocated" peakMegabytesAllocated
             record "GCStats/wallSeconds" wallSeconds
        record :: forall (m :: * -> *) a.
                  (Real a,Monad m,MonadIO m)
               => S.ByteString -> a -> m ()
        record path metric =
          liftIO (recordMetric path
                               (fromRational (toRational metric)))
