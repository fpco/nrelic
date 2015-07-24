{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as S
import           Network.Helics
import           System.Environment
import           System.IO.Error

main :: IO ()
main =
  do k:_ <- getArgs
     void (forkIO (sampler 60))
     withHelics
       (def {licenseKey = S.pack k})
       (do putStrLn "start"
           loop 0)
  where loop i =
          do withTransaction
               "test"
               def
               (\tid ->
                  catch (do withTransaction "inu"
                                            def
                                            (const (threadDelay (10 ^ 5)))
                            genericSegment autoScope
                                           "neko"
                                           (threadDelay (10 ^ 5))
                                           tid
                            when (i `mod` 97 == 0)
                                 (ioError (userError "user error!"))
                            when (i `mod` 101 == 0)
                                 (throwIO Overflow))
                        (\e ->
                           print (e :: SomeException)))
             threadDelay (2 * 10 ^ 5)
             loop (succ i)
