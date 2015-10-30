-- |
-- Module      :  Sound.HTagLib.Type
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions of types used to represent various tags and audio properties.

module Sound.HTagLib.Type
  ( Title
  , mkTitle
  , getTitle
  , Artist
  , mkArtist
  , getArtist
  , Album
  , mkAlbum
  , getAlbum
  , Comment
  , mkComment
  , getComment
  , Genre
  , mkGenre
  , getGenre
  , Year
  , mkYear
  , getYear
  , TrackNumber
  , mkTrackNumber
  , getTrackNumber
  , Duration
  , mkDuration
  , getDuration
  , BitRate
  , mkBitRate
  , getBitRate
  , SampleRate
  , mkSampleRate
  , getSampleRate
  , Channels
  , mkChannels
  , getChannels )
where

import Data.String

-- | Title tag.

newtype Title = Title
  { -- | Convert 'Title' to 'String'.
    getTitle :: String }
  deriving (Show, Eq, Ord)

instance IsString Title where
  fromString = mkTitle

-- | Construction of 'Title' type, null bytes are converted to spaces.

mkTitle :: String -> Title
mkTitle = Title . avoidNulls

-- | Artist tag.

newtype Artist = Artist
  { -- | Convert 'Artist' to 'String'.
    getArtist :: String }
  deriving (Show, Eq, Ord)

instance IsString Artist where
  fromString = mkArtist

-- | Construction of 'Artist' type, null bytes are converted to spaces.

mkArtist :: String -> Artist
mkArtist = Artist . avoidNulls

-- | Album tag.

newtype Album = Album
  { -- | Convert 'Album' to 'String'.
    getAlbum :: String }
  deriving (Show, Eq, Ord)

instance IsString Album where
  fromString = mkAlbum

-- | Construction of 'Album' type, null bytes are converted to spaces.

mkAlbum :: String -> Album
mkAlbum = Album . avoidNulls

-- | Comment tag.

newtype Comment = Comment
  { -- | Convert 'Comment' to 'String'.
    getComment :: String }
  deriving (Show, Eq, Ord)

instance IsString Comment where
  fromString = mkComment

-- | Construction of 'Comment' type, null bytes are converted to spaces.

mkComment :: String -> Comment
mkComment = Comment . avoidNulls

-- | Genre tag.

newtype Genre = Genre
  { -- | Convert 'Genre' to 'String'.
    getGenre :: String }
  deriving (Show, Eq, Ord)

instance IsString Genre where
  fromString = mkGenre

-- | Construction of 'Genre' type, null bytes are converted to spaces.

mkGenre :: String -> Genre
mkGenre = Genre . avoidNulls

-- | Year tag.

newtype Year = Year
  { -- | Convert 'Year' to 'Int'.
    getYear :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'Year' type, non-positive values result in 'Nothing'.

mkYear :: Int -> Maybe Year
mkYear = fmap Year . atLeast 1

-- | Track number tag.

newtype TrackNumber = TrackNumber
  { -- | Convert 'TrackNumber' to 'Int'.
    getTrackNumber :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'TrackNumber' type, non-positive values result in
-- 'Nothing'.

mkTrackNumber :: Int -> Maybe TrackNumber
mkTrackNumber = fmap TrackNumber . atLeast 1

-- | Duration in seconds.

newtype Duration = Duration
  { -- | Convert 'Duration' to 'Int'.
    getDuration :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'Duration' values, negative values result in 'Nothing'.

mkDuration :: Int -> Maybe Duration
mkDuration = fmap Duration . atLeast 0

-- | Bit rate in kb/s.

newtype BitRate = BitRate
  { -- | Convert 'BitRate' to 'Int'.
    getBitRate :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'BitRate' values, non-positive values result in
-- 'Nothing'.

mkBitRate :: Int -> Maybe BitRate
mkBitRate = fmap BitRate . atLeast 1

-- | Sample rate in Hz.

newtype SampleRate = SampleRate
  { -- | Convert 'SampleRate' to 'Int'.
    getSampleRate :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'SampleRate' values, non-positive values result in
-- 'Nothing'.

mkSampleRate :: Int -> Maybe SampleRate
mkSampleRate = fmap SampleRate . atLeast 1

-- | Number of channels in the audio stream.

newtype Channels = Channels
  { -- | Convert 'Channels' to 'Int'.
    getChannels :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'Channels' values, non-positive values result in
-- 'Nothing'.

mkChannels :: Int -> Maybe Channels
mkChannels = fmap Channels . atLeast 1

-- | Replace null bytes with spaces.

avoidNulls :: String -> String
avoidNulls = let f x = if x == '\0' then ' ' else x in fmap f

-- | @atLeast a b@ returns @Just b@ is @b@ is greater or equal to @a@,
-- otherwise result is @Nothing@.

atLeast :: Int -> Int -> Maybe Int
atLeast a b = if b >= a then Just b else Nothing
