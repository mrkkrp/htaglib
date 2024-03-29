{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Sound.HTagLib.Setter
-- Copyright   :  © 2015–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An interface for writing audio meta data. You don't need to import this
-- module directly, import "Sound.HTagLib" instead.
module Sound.HTagLib.Setter
  ( -- * High-level API
    TagSetter,
    setTags,
    setTags',

    -- * Built-in setters
    titleSetter,
    artistSetter,
    albumSetter,
    commentSetter,
    genreSetter,
    yearSetter,
    trackNumberSetter,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Data.Foldable (forM_)
import Sound.HTagLib.Internal qualified as I
import Sound.HTagLib.Type

-- | A composable entity that can be used together with the 'setTags' or the
-- 'setTags'' functions to write meta data to an audio file.
--
-- Note that in case of (for example):
--
-- > titleSetter "foo" <> titleSetter "bar"
--
-- The first value wins.
data TagSetter = TagSetter
  { sdTitle :: Maybe Title,
    sdArtist :: Maybe Artist,
    sdAlbum :: Maybe Album,
    sdComment :: Maybe Comment,
    sdGenre :: Maybe Genre,
    sdYear :: Maybe (Maybe Year),
    sdTrackNumber :: Maybe (Maybe TrackNumber)
  }

-- | @since 1.2.0
instance Semigroup TagSetter where
  x <> y =
    let f g = g x <|> g y
     in TagSetter
          { sdTitle = f sdTitle,
            sdArtist = f sdArtist,
            sdAlbum = f sdAlbum,
            sdComment = f sdComment,
            sdGenre = f sdGenre,
            sdYear = f sdYear,
            sdTrackNumber = f sdTrackNumber
          }

instance Monoid TagSetter where
  mempty =
    TagSetter
      { sdTitle = Nothing,
        sdArtist = Nothing,
        sdAlbum = Nothing,
        sdComment = Nothing,
        sdGenre = Nothing,
        sdYear = Nothing,
        sdTrackNumber = Nothing
      }
  mappend = (<>)

-- | Set tags in a specified file using the given setter.
--
-- Throws 'I.HTagLibException'.
setTags ::
  (MonadIO m) =>
  -- | Path to audio file
  FilePath ->
  -- | Encoding for ID3v2 frames
  Maybe ID3v2Encoding ->
  -- | Setter
  TagSetter ->
  m ()
setTags path enc = execSetter path enc Nothing

-- | Similar to 'setTags', but you can also specify the type of the audio
-- file explicitly (otherwise it's guessed from the file extension).
setTags' ::
  (MonadIO m) =>
  -- | Path to audio file
  FilePath ->
  -- | Encoding for ID3v2 frames
  Maybe ID3v2Encoding ->
  -- | Type of audio file
  FileType ->
  -- | Setter
  TagSetter ->
  m ()
setTags' path enc t = execSetter path enc (Just t)

-- | The most general way to set meta data. 'setTags' and 'setTags'' are
-- just wrappers around this function.
execSetter ::
  (MonadIO m) =>
  -- | Path to audio file
  FilePath ->
  -- | Encoding for ID3v2 frames
  Maybe ID3v2Encoding ->
  -- | Type of audio file (if known)
  Maybe FileType ->
  -- | Setter
  TagSetter ->
  m ()
execSetter path enc t TagSetter {..} = liftIO . I.withFile path t $ \fid -> do
  forM_ enc I.id3v2SetEncoding
  let writeTag x f = forM_ x (`f` fid)
  writeTag sdTitle I.setTitle
  writeTag sdArtist I.setArtist
  writeTag sdAlbum I.setAlbum
  writeTag sdComment I.setComment
  writeTag sdGenre I.setGenre
  writeTag sdYear I.setYear
  writeTag sdTrackNumber I.setTrackNumber
  I.saveFile path fid

-- | Setter for the track title.
titleSetter :: Title -> TagSetter
titleSetter x = mempty {sdTitle = Just x}

-- | Setter for the track artist.
artistSetter :: Artist -> TagSetter
artistSetter x = mempty {sdArtist = Just x}

-- | Setter for the track album.
albumSetter :: Album -> TagSetter
albumSetter x = mempty {sdAlbum = Just x}

-- | Setter for the track comment.
commentSetter :: Comment -> TagSetter
commentSetter x = mempty {sdComment = Just x}

-- | Setter for the track genre.
genreSetter :: Genre -> TagSetter
genreSetter x = mempty {sdGenre = Just x}

-- | Setter for the year tag, use 'Nothing' to clear the field.
yearSetter :: Maybe Year -> TagSetter
yearSetter x = mempty {sdYear = Just x}

-- | Setter for the track number, use 'Nothing' to clear the field.
trackNumberSetter :: Maybe TrackNumber -> TagSetter
trackNumberSetter x = mempty {sdTrackNumber = Just x}
