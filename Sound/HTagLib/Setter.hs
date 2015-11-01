-- |
-- Module      :  Sound.HTagLib.Setter
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- High-level interface for writing audio meta data. You don't need to
-- import this module directly, import "Sound.HTagLib" instead.

{-# LANGUAGE CPP #-}

module Sound.HTagLib.Setter
  ( TagSetter
  , setTags
  , setTags'
  , titleSetter
  , artistSetter
  , albumSetter
  , commentSetter
  , genreSetter
  , yearSetter
  , trackNumberSetter )
where

import Sound.HTagLib.Type
import qualified Sound.HTagLib.Internal as I

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-- | Composable entity that can be used together with 'setTags' or
-- 'setTags'' to write meta data to audio file.

newtype TagSetter = TagSetter { runSetter :: I.FileId -> IO () }

instance Monoid TagSetter where
  mempty  = TagSetter $ const (return ())
  x `mappend` y = TagSetter $ \fid ->
                    do runSetter x fid
                       runSetter y fid

-- | Set tags in specified file using given setter.
--
-- In case of trouble 'I.HTagLibException' will be thrown.

setTags :: FilePath              -- ^ Path to audio file
        -> Maybe I.ID3v2Encoding -- ^ Encoding for ID3v2 frames
        -> TagSetter             -- ^ Setter
        -> IO ()
setTags path enc = execSetter path enc Nothing

-- | Similar to 'setTags', but you can also specify type of audio file
-- explicitly (otherwise it's guessed from file extension).

setTags' :: FilePath              -- ^ Path to audio file
         -> Maybe I.ID3v2Encoding -- ^ Encoding for ID3v2 frames
         -> I.FileType            -- ^ Type of audio file
         -> TagSetter             -- ^ Setter
         -> IO ()
setTags' path enc t = execSetter path enc (Just t)

-- | The most general way to set meta data. 'setTags' and 'setTags'' are
-- just wrappers around this function.

execSetter :: FilePath               -- ^ Path to audio file
           -> Maybe I.ID3v2Encoding  -- ^ Encoding for ID3v2 frames
           -> Maybe I.FileType       -- ^ Type of audio file (if known)
           -> TagSetter              -- ^ Setter
           -> IO ()
execSetter path enc t s = I.withFile path t $ \fid -> do
  case enc of
    Nothing -> return ()
    Just e  -> I.id3v2SetEncoding e
  runSetter s fid
  I.saveFile path fid

-- | Setter for track title.

titleSetter :: Title -> TagSetter
titleSetter = TagSetter . I.setTitle

-- | Setter for track artist.

artistSetter :: Artist -> TagSetter
artistSetter = TagSetter . I.setArtist

-- | Setter for track album.

albumSetter :: Album -> TagSetter
albumSetter = TagSetter . I.setAlbum

-- | Setter for track comment.

commentSetter :: Comment -> TagSetter
commentSetter = TagSetter . I.setComment

-- | Setter for track genre.

genreSetter :: Genre -> TagSetter
genreSetter = TagSetter . I.setGenre

-- | Setter for year tag, use 'Nothing' to clear the field.

yearSetter :: Maybe Year -> TagSetter
yearSetter = TagSetter . I.setYear

-- | Setter for track number, use 'Nothing' to clear the field.

trackNumberSetter :: Maybe TrackNumber -> TagSetter
trackNumberSetter = TagSetter . I.setTrackNumber
