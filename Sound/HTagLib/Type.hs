-- |
-- Module      :  Sound.HTagLib.Type
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions of types used to represent various tags and audio properties.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Sound.HTagLib.Type
  ( Title
  , mkTitle
  , unTitle
  , Artist
  , mkArtist
  , unArtist
  , Album
  , mkAlbum
  , unAlbum
  , Comment
  , mkComment
  , unComment
  , Genre
  , mkGenre
  , unGenre
  , Year
  , mkYear
  , unYear
  , TrackNumber
  , mkTrackNumber
  , unTrackNumber
  , Duration
  , mkDuration
  , unDuration
  , BitRate
  , mkBitRate
  , unBitRate
  , SampleRate
  , mkSampleRate
  , unSampleRate
  , Channels
  , mkChannels
  , unChannels
  , FileType (..)
  , ID3v2Encoding (..)
  , HTagLibException (..) )
where

import Control.Exception (Exception)
import Data.String
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Text as T

-- | Title tag.

newtype Title = Title Text deriving (Show, Eq, Ord)

instance IsString Title where
  fromString = mkTitle . fromString

-- | Construction of 'Title' type, null bytes are converted to spaces.

mkTitle :: Text -> Title
mkTitle = Title . avoidNulls

-- | Convert 'Title' to 'Text'.

unTitle :: Title -> Text
unTitle (Title x) = x

-- | Artist tag.

newtype Artist = Artist Text deriving (Show, Eq, Ord)

instance IsString Artist where
  fromString = mkArtist . fromString

-- | Construction of 'Artist' type, null bytes are converted to spaces.

mkArtist :: Text -> Artist
mkArtist = Artist . avoidNulls

-- | Convert 'Artist' to 'Text'.

unArtist :: Artist -> Text
unArtist (Artist x) = x

-- | Album tag.

newtype Album = Album Text deriving (Show, Eq, Ord)

instance IsString Album where
  fromString = mkAlbum . fromString

-- | Construction of 'Album' type, null bytes are converted to spaces.

mkAlbum :: Text -> Album
mkAlbum = Album . avoidNulls

-- | Convert 'Album' to 'Text'.

unAlbum :: Album -> Text
unAlbum (Album x) = x

-- | Comment tag.

newtype Comment = Comment Text deriving (Show, Eq, Ord)

instance IsString Comment where
  fromString = mkComment . fromString

-- | Construction of 'Comment' type, null bytes are converted to spaces.

mkComment :: Text -> Comment
mkComment = Comment . avoidNulls

-- | Convert 'Comment' to 'Text'.

unComment :: Comment -> Text
unComment (Comment x) = x

-- | Genre tag.

newtype Genre = Genre Text deriving (Show, Eq, Ord)

instance IsString Genre where
  fromString = mkGenre . fromString

-- | Construction of 'Genre' type, null bytes are converted to spaces.

mkGenre :: Text -> Genre
mkGenre = Genre . avoidNulls

-- | Convert 'Genre' to 'Text'.

unGenre :: Genre -> Text
unGenre (Genre x) = x

-- | Year tag.

newtype Year = Year Int deriving (Show, Eq, Ord)

-- | Construction of 'Year' type, non-positive values result in 'Nothing'.

mkYear :: Int -> Maybe Year
mkYear = fmap Year . atLeast 1

-- | Convert 'Year' to 'Int'.

unYear :: Year -> Int
unYear (Year x) = x

-- | Track number tag.

newtype TrackNumber = TrackNumber Int deriving (Show, Eq, Ord)

-- | Construction of 'TrackNumber' type, non-positive values result in
-- 'Nothing'.

mkTrackNumber :: Int -> Maybe TrackNumber
mkTrackNumber = fmap TrackNumber . atLeast 1

-- | Convert 'TrackNumber' to 'Int'.

unTrackNumber :: TrackNumber -> Int
unTrackNumber (TrackNumber x) = x

-- | Duration in seconds.

newtype Duration = Duration Int deriving (Show, Eq, Ord)

-- | Construction of 'Duration' values, negative values result in 'Nothing'.

mkDuration :: Int -> Maybe Duration
mkDuration = fmap Duration . atLeast 0

-- | Convert 'Duration' to 'Int'.

unDuration :: Duration -> Int
unDuration (Duration x) = x

-- | Bit rate in kb/s.

newtype BitRate = BitRate Int deriving (Show, Eq, Ord)

-- | Construction of 'BitRate' values, negative values result in
-- 'Nothing'.

mkBitRate :: Int -> Maybe BitRate
mkBitRate = fmap BitRate . atLeast 0

-- | Convert 'BitRate' to 'Int'.

unBitRate :: BitRate -> Int
unBitRate (BitRate x) = x

-- | Sample rate in Hz.

newtype SampleRate = SampleRate Int deriving (Show, Eq, Ord)

-- | Construction of 'SampleRate' values, non-positive values result in
-- 'Nothing'.

mkSampleRate :: Int -> Maybe SampleRate
mkSampleRate = fmap SampleRate . atLeast 1

-- | Convert 'SampleRate' to 'Int'.

unSampleRate :: SampleRate -> Int
unSampleRate (SampleRate x) = x

-- | Number of channels in the audio stream.

newtype Channels = Channels Int deriving (Show, Eq, Ord)

-- | Construction of 'Channels' values, non-positive values result in
-- 'Nothing'.

mkChannels :: Int -> Maybe Channels
mkChannels = fmap Channels . atLeast 1

-- | Convert 'Channels' to 'Int'.

unChannels :: Channels -> Int
unChannels (Channels x) = x

-- | Replace null bytes with spaces.

avoidNulls :: Text -> Text
avoidNulls = T.replace "\0" " "

-- | @atLeast a b@ returns @Just b@ is @b@ is greater or equal to @a@,
-- otherwise result is @Nothing@.

atLeast :: Int -> Int -> Maybe Int
atLeast a b = if b >= a then Just b else Nothing

-- | Types of files TagLib can work with. This may be used to explicitly
-- specify type of file rather than relying on TagLib ability to guess type
-- of file from its extension.

data FileType
  = MPEG               -- ^ MPEG
  | OggVorbis          -- ^ Ogg vorbis
  | FLAC               -- ^ FLAC
  | MPC                -- ^ MPC
  | OggFlac            -- ^ Ogg FLAC
  | WavPack            -- ^ Wav pack
  | Speex              -- ^ Speex
  | TrueAudio          -- ^ True audio
  | MP4                -- ^ MP4
  | ASF                -- ^ ASF
    deriving (Show, Eq, Enum)

-- | Encoding for ID3v2 frames that are written to tags.

data ID3v2Encoding
  = ID3v2Latin1        -- ^ Latin1
  | ID3v2UTF16         -- ^ UTF-16
  | ID3v2UTF16BE       -- ^ UTF-16 big endian
  | ID3v2UTF8          -- ^ UTF-8
    deriving (Show, Eq, Enum)

-- | The data type represents exceptions specific to the library.

data HTagLibException
  = OpeningFailed FilePath
    -- ^ Attempt to open audio file to read its tags failed
  | InvalidFile FilePath
    -- ^ File can be opened, but it doesn't contain any information that can
    -- be interpreted by the library
  | SavingFailed FilePath
    -- ^ Saving failed
    deriving (Eq, Show, Typeable)

instance Exception HTagLibException
