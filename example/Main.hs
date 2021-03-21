{-# LANGUAGE RankNTypes #-}

-- | An example usage of Pipes.PulseSimple
module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary.Get (getInt16le, isEmpty, runGet)
import Data.Binary.Put (putInt16le, runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Float (int2Float)
import GHC.Int (Int16)
import Pipes (Consumer', Producer', await, runEffect, yield, (>->))
import Pipes.PulseSimple (readPulse, writePulse)
import Pipes.Safe (runSafeT)
import System.Environment (getArgs)

-- | Binary decoder
decodeSampleList :: BS.ByteString -> [Int16]
decodeSampleList = runGet get . fromStrict
  where
    get = do
      empty <- isEmpty
      if empty
        then return []
        else do
          sample <- getInt16le
          rest <- get
          return (sample : rest)

-- | Binary encoder
encodeSampleList :: [Int16] -> BS.ByteString
encodeSampleList = toStrict . runPut . put
  where
    put [] = pure ()
    put (x : xs) = putInt16le x >> put xs

-- | Print `Noise!` when signal is detected
consume :: MonadIO m => UTCTime -> Consumer' BS.ByteString m ()
consume prev = do
  now <- liftIO getCurrentTime
  liftIO $ putStrLn $ "Last buffer received " <> show (diffUTCTime now prev) <> " second ago"
  samples <- decodeSampleList <$> await
  when (maximum samples > 1000) (liftIO (putStrLn "Noise!"))
  consume now

generateTone :: Int -> Int -> [Int16]
generateTone pos size = go 0
  where
    note = 440
    maxInt16 :: Int16
    maxInt16 = maxBound
    scale :: Float
    scale = int2Float (fromIntegral (maxInt16 `div` 5))
    sin16 :: Int -> Int16
    sin16 x = round $ (* scale) $ sin $ note * 2 * pi * int2Float x / 44100
    go :: Int -> [Int16]
    go n
      | n == size = []
      | otherwise = sin16 (n + pos) : go (n + 1)

-- | Create a tone
produce :: MonadIO m => UTCTime -> Producer' BS.ByteString m ()
produce = go 0
  where
    size = 1764
    go pos prev = do
      now <- liftIO getCurrentTime
      liftIO $ putStrLn $ "Last buffer sent " <> show (diffUTCTime now prev) <> " second ago"
      let samples = generateTone pos size
      let buf = encodeSampleList samples
      yield buf
      go (pos + size) now

main :: IO ()
main = do
  args <- getArgs
  now <- getCurrentTime
  runSafeT $
    runEffect $ case args of
      ["record"] -> readPulse "test" Nothing 25 >-> consume now
      ["play"] -> produce now >-> writePulse "test" Nothing 25
      _ -> error "usage: record|play"
