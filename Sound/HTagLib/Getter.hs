{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  Sound.HTagLib.Getter
-- Copyright   :  © 2015–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An applicative interface for reading of audio meta data. You don't need
-- to import this module directly, import "Sound.HTagLib" instead.
module Sound.HTagLib.Getter
  ( -- * High-level API
    TagGetter,
    getTags,
    getTags',

    -- * Built-in getters
    titleGetter,
    artistGetter,
    albumGetter,
    commentGetter,
    genreGetter,
    yearGetter,
    trackNumberGetter,
    durationGetter,
    bitRateGetter,
    sampleRateGetter,
    channelsGetter,
  )
where

import Control.Monad.IO.Class
import Sound.HTagLib.Internal qualified as I
import Sound.HTagLib.Type

-- | A composable entity that can be passed to the 'getTags' or 'getTags''
-- functions to read multiple meta data fields at once.
newtype TagGetter a = TagGetter {runGetter :: I.FileId -> IO a}
  deriving (Functor)

instance Applicative TagGetter where
  pure = TagGetter . const . return
  x <*> y = TagGetter $ \fid -> do
    f <- runGetter x fid
    f <$> runGetter y fid

-- | @getTags path g@ will try to read the file located at @path@ and read
-- the meta data of the file using the getter @g@. The type of the file will
-- be guessed from its extension. If this is not satisfactory and you want
-- to explicitly specify the file type, see 'getTags'' variation of this
-- function.
--
-- Throws 'I.HTagLibException'.
getTags ::
  (MonadIO m) =>
  -- | Path to audio file
  FilePath ->
  -- | Getter
  TagGetter a ->
  -- | Extracted data
  m a
getTags path = execGetter path Nothing

-- | This is essentially the same as 'getTags', but allows us to explicitly
-- choose file type (see 'FileType').
getTags' ::
  (MonadIO m) =>
  -- | Path to audio file
  FilePath ->
  -- | Type of audio file
  FileType ->
  -- | Getter
  TagGetter a ->
  -- | Extracted data
  m a
getTags' path t = execGetter path (Just t)

-- | This is the most general way to read meta data from file. 'getTags' and
-- 'getTags'' are just wrappers around the function.
execGetter ::
  (MonadIO m) =>
  -- | Path to audio file
  FilePath ->
  -- | Type of audio file (if known)
  Maybe FileType ->
  -- | Getter
  TagGetter a ->
  -- | Extracted data
  m a
execGetter path t = liftIO . I.withFile path t . runGetter

-- | Getter to retrieve the track title.
titleGetter :: TagGetter Title
titleGetter = TagGetter I.getTitle

-- | Getter to retrieve the track artist.
artistGetter :: TagGetter Artist
artistGetter = TagGetter I.getArtist

-- | Getter to retrieve the track album.
albumGetter :: TagGetter Album
albumGetter = TagGetter I.getAlbum

-- | Getter to retrieve the track comment.
commentGetter :: TagGetter Comment
commentGetter = TagGetter I.getComment

-- | Getter to retrieve the genre of the track.
genreGetter :: TagGetter Genre
genreGetter = TagGetter I.getGenre

-- | Getter to retrieve the year of the track (returns 'Nothing' if the data
-- is missing).
yearGetter :: TagGetter (Maybe Year)
yearGetter = TagGetter I.getYear

-- | Getter to retrieve the track number (returns 'Nothing' if the data is
-- missing).
trackNumberGetter :: TagGetter (Maybe TrackNumber)
trackNumberGetter = TagGetter I.getTrackNumber

-- | Getter to retrieve the duration in seconds.
durationGetter :: TagGetter Duration
durationGetter = TagGetter I.getDuration

-- | Getter to retrieve the bit rate.
bitRateGetter :: TagGetter BitRate
bitRateGetter = TagGetter I.getBitRate

-- | Getter to retrieve the sample rate.
sampleRateGetter :: TagGetter SampleRate
sampleRateGetter = TagGetter I.getSampleRate

-- | Getter to retrieve the number of channels of the audio data.
channelsGetter :: TagGetter Channels
channelsGetter = TagGetter I.getChannels
