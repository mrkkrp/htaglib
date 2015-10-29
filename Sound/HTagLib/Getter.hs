-- |
-- Module      :  Sound.HTagLib.Getter
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- High-level applicative interface for reading of audio meta data. You
-- don't need to import this module directly, import "Sound.HTagLib"
-- instead.

module Sound.HTagLib.Getter
  ( TagGetter
  , getTags
  , getTags'
    -- * Built-in getters
  , titleGetter
  , artistGetter
  , albumGetter
  , commentGetter
  , genreGetter
  , yearGetter
  , trackNumberGetter
  , durationGetter
  , bitRateGetter
  , sampleRateGetter
  , channelsGetter )
where

import Sound.HTagLib.Type
import qualified Sound.HTagLib.Internal as I

-- | This type represents composable entity that can be used with 'getTags'
-- or 'getTags'' functions to read batch of meta parameters.

newtype TagGetter a = TagGetter { runGetter :: I.FileId -> IO a }

instance Functor TagGetter where
  fmap f x = TagGetter $ \fid -> f <$> runGetter x fid

instance Applicative TagGetter where
  pure    = TagGetter . const . return
  x <*> y = TagGetter $ \fid ->
              do f <- runGetter x fid
                 f <$> runGetter y fid

-- | @getTags path g@ will try to read file located at @path@ and read meta
-- data of the file using getter @g@. Type of file will be guessed from its
-- extension. If this is no satisfactory and you want to explicitly specify
-- file type, see 'getTags'' variation of this function.
--
-- If the file cannot be found at @path@ or user has no permission to read
-- it 'IOException' will be thrown.

getTags :: FilePath    -- ^ Path to audio file
        -> TagGetter a -- ^ Getter
        -> IO a        -- ^ Extracted data
getTags path = execGetter path Nothing

-- | This is essentially the same as 'getTags', but allows to explicitly
-- choose file type (see 'I.FileType').

getTags' :: FilePath    -- ^ Path to audio file
         -> I.FileType  -- ^ Type of
         -> TagGetter a -- ^ Getter
         -> IO a
getTags' path t = execGetter path (Just t)

execGetter :: FilePath -> Maybe I.FileType -> TagGetter a -> IO a
execGetter path t g = do
  fid <- I.newFile path t
  x   <- runGetter g fid
  I.freeFile fid
  return x

-- | Getter to retrieve track title.

titleGetter :: TagGetter Title
titleGetter = TagGetter I.getTitle

-- | Getter to retrieve track artist.

artistGetter :: TagGetter Artist
artistGetter = TagGetter I.getArtist

-- | Getter to retrieve track album.

albumGetter :: TagGetter Album
albumGetter = TagGetter I.getAlbum

-- | Getter to retrieve track comment.

commentGetter :: TagGetter Comment
commentGetter = TagGetter I.getComment

-- | Getter to retrieve genre of the track.

genreGetter :: TagGetter Genre
genreGetter = TagGetter I.getGenre

-- | Getter to retrieve year to the track (returns 'Nothing' if the data is
-- missing).

yearGetter :: TagGetter (Maybe Year)
yearGetter = TagGetter I.getYear

-- | Getter to retrieve track number.

trackNumberGetter :: TagGetter (Maybe TrackNumber)
trackNumberGetter = TagGetter I.getTrackNumber

-- | Getter to retrieve duration in seconds.

durationGetter :: TagGetter Duration
durationGetter = TagGetter I.getDuration

-- | Getter to retrieve bit rate.

bitRateGetter :: TagGetter BitRate
bitRateGetter = TagGetter I.getBitRate

-- | Getter to retrieve sample rate.

sampleRateGetter :: TagGetter SampleRate
sampleRateGetter = TagGetter I.getSampleRate

-- | Getter to retrieve number of channels in audio data.

channelsGetter :: TagGetter Channels
channelsGetter = TagGetter I.getChannels
