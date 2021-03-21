{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: (c) 2021 Tristan de Cacqueray
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- Pipes for simple-pulse audio
module Pipes.PulseSimple
  ( readPulse,
    writePulse,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Pipes (Consumer', Producer', await, yield)
import Pipes.Safe (MonadSafe, bracket)
import Sound.Pulse.Simple
  ( BufferAttr (..),
    Direction (..),
    Endian (..),
    SampleFormat (..),
    SampleSpec (..),
    Simple,
    simpleFree,
    simpleNew,
    simpleReadRaw,
    simpleWriteRaw,
  )

defaultSampleSpec :: SampleSpec
defaultSampleSpec = SampleSpec (S16 LittleEndian) 44100 1

-- | Return the bits count of a single sample
sampleSize :: SampleFormat -> Int
sampleSize = \case
  U8 _ -> 1
  S16 _ -> 2
  S24 _ -> 3
  S2432 _ -> 4
  S32 _ -> 4
  F32 _ -> 4

-- | Return the bits count of an one second buffer
bufferSize :: SampleSpec -> Int
bufferSize (SampleSpec fmt sampling chan) = sampleSize fmt * sampling * chan

-- | Compute the sample spec and buffer size for a given fps
specSize :: Maybe SampleSpec -> Int -> (SampleSpec, Int)
specSize specm fps = (spec, size)
  where
    spec = fromMaybe defaultSampleSpec specm
    size = bufferSize spec `div` fps

newClient :: MonadIO m => String -> Direction -> SampleSpec -> Int -> m Simple
newClient name dir spec size = liftIO create
  where
    create = simpleNew Nothing name dir Nothing "pulse-pipe" spec Nothing bufAttr
    bufAttr = Just (BufferAttr (Just size) (Just size) (Just size) (Just size) (Just size))

freeClient :: MonadIO m => Simple -> m ()
freeClient = liftIO . simpleFree

-- | Create an audio pipe producer that yields `fps` bytestring buffer per second
readPulse ::
  MonadSafe m =>
  -- | The client name
  String ->
  -- | The sample spec, default to mono 44100 Hz 16 bit signed integer
  Maybe SampleSpec ->
  -- | Frames per second
  Int ->
  -- | The pipe producing bytestring
  Producer' ByteString m ()
readPulse name specm fps = bracket (newClient name Record spec size) freeClient producePulse
  where
    (spec, size) = specSize specm fps
    producePulse s = do
      buf <- liftIO (simpleReadRaw s size)
      yield buf
      producePulse s
{-# INLINEABLE readPulse #-}

-- | Create an audio pipe consumer that awaits `fps` bytestring buffer per second
writePulse ::
  MonadSafe m =>
  -- | The client name
  String ->
  -- | The sample spec, default to mono 44100 Hz 16 bit signed integer
  Maybe SampleSpec ->
  -- | Frames per second
  Int ->
  -- | The pipe consuming bytestring
  Consumer' ByteString m ()
writePulse name specm fps = bracket (newClient name Play spec size) freeClient consumePulse
  where
    (spec, size) = specSize specm fps
    consumePulse s = do
      buf <- await
      liftIO (simpleWriteRaw s buf)
      consumePulse s
{-# INLINEABLE writePulse #-}
