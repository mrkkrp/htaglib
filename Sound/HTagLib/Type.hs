-- |
-- Module      :  Sound.HTagLib.Type
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD3
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
  { getTitle :: String }
  deriving (Show, Eq, Ord)

instance IsString Title where
  fromString = mkTitle

-- | Construction of 'Title' type, null bytes are converted to spaces.

mkTitle :: String -> Title
mkTitle = Title . avoidNulls

-- | Artist tag.

newtype Artist = Artist
  { getArtist :: String }
  deriving (Show, Eq, Ord)

instance IsString Artist where
  fromString = mkArtist

-- | Construction of 'Artist' type, null bytes are converted to spaces.

mkArtist :: String -> Artist
mkArtist = Artist . avoidNulls

-- | Album tag.

newtype Album = Album
  { getAlbum :: String }
  deriving (Show, Eq, Ord)

instance IsString Album where
  fromString = mkAlbum

-- | Construction of 'Album' type, null bytes are converted to spaces.

mkAlbum :: String -> Album
mkAlbum = Album . avoidNulls

-- | Comment tag.

newtype Comment = Comment
  { getComment :: String }
  deriving (Show, Eq, Ord)

instance IsString Comment where
  fromString = mkComment

-- | Construction of 'Comment' type, null bytes are converted to spaces.

mkComment :: String -> Comment
mkComment = Comment . avoidNulls

-- | Genre tag.

newtype Genre = Genre
  { getGenre :: String }
  deriving (Show, Eq, Ord)

instance IsString Genre where
  fromString = mkGenre

-- | Construction of 'Genre' type, null bytes are converted to spaces.

mkGenre :: String -> Genre
mkGenre = Genre . avoidNulls

-- | Year tag.

newtype Year = Year
  { getYear :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'Year' type, non-positive values result in 'Nothing'.

mkYear :: Int -> Maybe Year
mkYear = fmap Year . atLeast 1

-- | Track number tag.

newtype TrackNumber = TrackNumber
  { getTrackNumber :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'TrackNumber' type, non-positive values result in
-- 'Nothing'.

mkTrackNumber :: Int -> Maybe TrackNumber
mkTrackNumber = fmap TrackNumber . atLeast 1

-- | Duration in seconds.

newtype Duration = Duration
  { getDuration :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'Duration' values, negative values result in 'Nothing'.

mkDuration :: Int -> Maybe Duration
mkDuration = fmap Duration . atLeast 0

-- | Bit rate in kb/s.

newtype BitRate = BitRate
  { getBitRate :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'BitRate' values, non-positive values result in
-- 'Nothing'.

mkBitRate :: Int -> Maybe BitRate
mkBitRate = fmap BitRate . atLeast 1

-- | Sample rate in Hz.

newtype SampleRate = SampleRate
  { getSampleRate :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'SampleRate' values, non-positive values result in
-- 'Nothing'.

mkSampleRate :: Int -> Maybe SampleRate
mkSampleRate = fmap SampleRate . atLeast 1

-- | Number of channels in the audio stream.

newtype Channels = Channels
  { getChannels :: Int }
  deriving (Show, Eq, Ord)

-- | Construction of 'Channels' values, non-positive values result in
-- 'Nothing'.

mkChannels :: Int -> Maybe Channels
mkChannels = fmap Channels . atLeast 1

avoidNulls :: String -> String
avoidNulls = let f x = if x == '\0' then ' ' else x in fmap f

atLeast :: Int -> Int -> Maybe Int
atLeast a x = if x >= a then Just x else Nothing
