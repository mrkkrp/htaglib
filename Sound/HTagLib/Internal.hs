{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      :  Sound.HTagLib.Internal
-- Copyright   :  © 2015–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level interaction with the underlying C API. You don't want to use
-- this, see "Sound.HTagLib" instead.
module Sound.HTagLib.Internal
  ( -- * Data types
    FileId,

    -- * File API
    withFile,
    saveFile,

    -- * Tag API
    getTitle,
    getArtist,
    getAlbum,
    getComment,
    getGenre,
    getYear,
    getTrackNumber,
    setTitle,
    setArtist,
    setAlbum,
    setComment,
    setGenre,
    setYear,
    setTrackNumber,

    -- * Audio properties API
    getDuration,
    getBitRate,
    getSampleRate,
    getChannels,

    -- * Special convenience ID3v2 functions
    id3v2SetEncoding,
  )
where

import Control.Exception (bracket, throw)
import Control.Monad (unless, when)
import Data.ByteString (packCString, useAsCString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Sound.HTagLib.Type qualified as T

data TagLibFile

data TagLibTag

data TagLibProperties

-- | This is an abstraction representing opened file. Other modules can
-- pass it around and treat it like a black box.
newtype FileId = FileId (Ptr TagLibFile)

----------------------------------------------------------------------------
-- Misc

foreign import ccall unsafe "taglib/tag_c.h taglib_set_string_management_enabled"
  c_taglib_set_string_management_enabled :: CInt -> IO ()

-- File API

foreign import ccall unsafe "taglib/tag_c.h taglib_file_new"
  c_taglib_file_new :: CString -> IO (Ptr TagLibFile)

foreign import ccall unsafe "taglib/tag_c.h taglib_file_new_type"
  c_taglib_file_new_type :: CString -> CInt -> IO (Ptr TagLibFile)

foreign import ccall unsafe "taglib/tag_c.h taglib_file_free"
  c_taglib_file_free :: Ptr TagLibFile -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_file_is_valid"
  c_taglib_file_is_valid :: Ptr TagLibFile -> IO CInt

foreign import ccall unsafe "taglib/tag_c.h taglib_file_tag"
  c_taglib_file_tag :: Ptr TagLibFile -> IO (Ptr TagLibTag)

foreign import ccall unsafe "taglib/tag_c.h taglib_file_audioproperties"
  c_taglib_file_properties :: Ptr TagLibFile -> IO (Ptr TagLibProperties)

foreign import ccall unsafe "taglib/tag_c.h taglib_file_save"
  c_taglib_file_save :: Ptr TagLibFile -> IO CInt

----------------------------------------------------------------------------
-- Tag API

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_title"
  c_taglib_tag_title :: Ptr TagLibTag -> IO CString

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_artist"
  c_taglib_tag_artist :: Ptr TagLibTag -> IO CString

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_album"
  c_taglib_tag_album :: Ptr TagLibTag -> IO CString

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_comment"
  c_taglib_tag_comment :: Ptr TagLibTag -> IO CString

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_genre"
  c_taglib_tag_genre :: Ptr TagLibTag -> IO CString

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_year"
  c_taglib_tag_year :: Ptr TagLibTag -> IO CUInt

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_track"
  c_taglib_tag_track :: Ptr TagLibTag -> IO CUInt

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_title"
  c_taglib_tag_set_title :: Ptr TagLibTag -> CString -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_artist"
  c_taglib_tag_set_artist :: Ptr TagLibTag -> CString -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_album"
  c_taglib_tag_set_album :: Ptr TagLibTag -> CString -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_comment"
  c_taglib_tag_set_comment :: Ptr TagLibTag -> CString -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_genre"
  c_taglib_tag_set_genre :: Ptr TagLibTag -> CString -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_year"
  c_taglib_tag_set_year :: Ptr TagLibTag -> CUInt -> IO ()

foreign import ccall unsafe "taglib/tag_c.h taglib_tag_set_track"
  c_taglib_tag_set_track :: Ptr TagLibTag -> CUInt -> IO ()

----------------------------------------------------------------------------
-- Audio properties API

foreign import ccall unsafe "taglib/tag_c.h taglib_audioproperties_length"
  c_taglib_properties_length :: Ptr TagLibProperties -> IO CInt

foreign import ccall unsafe "taglib/tag_c.h taglib_audioproperties_bitrate"
  c_taglib_properties_bitrate :: Ptr TagLibProperties -> IO CInt

foreign import ccall unsafe "taglib/tag_c.h taglib_audioproperties_samplerate"
  c_taglib_properties_samplerate :: Ptr TagLibProperties -> IO CInt

foreign import ccall unsafe "taglib/tag_c.h taglib_audioproperties_channels"
  c_taglib_properties_channels :: Ptr TagLibProperties -> IO CInt

----------------------------------------------------------------------------
-- Special convenience ID3v2 functions

foreign import ccall unsafe "taglib/tag_c.h taglib_id3v2_set_default_text_encoding"
  c_taglib_id3v2_set_default_text_encoding :: CInt -> IO ()

----------------------------------------------------------------------------
-- File API

-- | Open an audio file and return its ID (an opaque type that the rest of
-- library can pass around).
--
-- Throws 'IOException'.
newFile ::
  -- | Path to audio file
  FilePath ->
  -- | Type of file (or it will be guessed)
  Maybe T.FileType ->
  -- | Id to pass around
  IO FileId
newFile path ftype = do
  c_taglib_set_string_management_enabled $ fromBool False
  ptr <- withCString path $ \cstr ->
    case ftype of
      Nothing -> c_taglib_file_new cstr
      Just t -> c_taglib_file_new_type cstr (enumToCInt t)
  when (ptr == nullPtr) $
    throw (T.OpeningFailed path)
  valid <- toBool <$> c_taglib_file_is_valid ptr
  unless valid $
    throw (T.InvalidFile path)
  return (FileId ptr)

-- | Free file given its ID. Every time you open a file, free it.
freeFile :: FileId -> IO ()
freeFile (FileId ptr) = c_taglib_file_free ptr

-- | Open an audio file located at the specified path, execute some actions
-- given its 'FileId' and then free the file.
withFile ::
  -- | Path to audio file
  FilePath ->
  -- | Type of file (or it will be guessed)
  Maybe T.FileType ->
  -- | Computation depending of 'FileId'
  (FileId -> IO a) ->
  -- | Result value
  IO a
withFile path t = bracket (newFile path t) freeFile

-- | Save the file given its ID. Given 'FilePath' just tells what to mention
-- in the exception messages if the action fails, it doesn't specify where
-- to save the file (it's determined by the 'FileId').
saveFile ::
  -- | File name to use in exceptions
  FilePath ->
  -- | File identifier
  FileId ->
  IO ()
saveFile path (FileId ptr) = do
  success <- toBool <$> c_taglib_file_save ptr
  unless success $
    throw (T.SavingFailed path)

----------------------------------------------------------------------------
-- Tag API

-- | Get the title tag.
getTitle :: FileId -> IO T.Title
getTitle = fmap T.mkTitle . getStrValue c_taglib_tag_title

-- | Get the artist tag.
getArtist :: FileId -> IO T.Artist
getArtist = fmap T.mkArtist . getStrValue c_taglib_tag_artist

-- | Get the album tag.
getAlbum :: FileId -> IO T.Album
getAlbum = fmap T.mkAlbum . getStrValue c_taglib_tag_album

-- | Get the comment tag.
getComment :: FileId -> IO T.Comment
getComment = fmap T.mkComment . getStrValue c_taglib_tag_comment

-- | Get the genre tag.
getGenre :: FileId -> IO T.Genre
getGenre = fmap T.mkGenre . getStrValue c_taglib_tag_genre

-- | Get the year tag.
getYear :: FileId -> IO (Maybe T.Year)
getYear = fmap T.mkYear . getIntValue c_taglib_tag_year

-- | Get the track number.
getTrackNumber :: FileId -> IO (Maybe T.TrackNumber)
getTrackNumber = fmap T.mkTrackNumber . getIntValue c_taglib_tag_track

-- | Set the title tag.
setTitle :: T.Title -> FileId -> IO ()
setTitle v = setStrValue c_taglib_tag_set_title (T.unTitle v)

-- | Set the artist tag.
setArtist :: T.Artist -> FileId -> IO ()
setArtist v = setStrValue c_taglib_tag_set_artist (T.unArtist v)

-- | Set the album tag.
setAlbum :: T.Album -> FileId -> IO ()
setAlbum v = setStrValue c_taglib_tag_set_album (T.unAlbum v)

-- | Set the comment tag.
setComment :: T.Comment -> FileId -> IO ()
setComment v = setStrValue c_taglib_tag_set_comment (T.unComment v)

-- | Set the genre tag.
setGenre :: T.Genre -> FileId -> IO ()
setGenre v = setStrValue c_taglib_tag_set_genre (T.unGenre v)

-- | Set the year tag.
setYear :: Maybe T.Year -> FileId -> IO ()
setYear v = setIntValue c_taglib_tag_set_year (T.unYear <$> v)

-- | Set the track number tag.
setTrackNumber :: Maybe T.TrackNumber -> FileId -> IO ()
setTrackNumber v = setIntValue c_taglib_tag_set_track (T.unTrackNumber <$> v)

----------------------------------------------------------------------------
-- Audio properties API

-- | Get the duration.
getDuration :: FileId -> IO T.Duration
getDuration =
  fmap (fromJust . T.mkDuration)
    . getIntProperty c_taglib_properties_length

-- | Get the bit rate.
getBitRate :: FileId -> IO T.BitRate
getBitRate =
  fmap (fromJust . T.mkBitRate)
    . getIntProperty c_taglib_properties_bitrate

-- | Get the sample rate.
getSampleRate :: FileId -> IO T.SampleRate
getSampleRate =
  fmap (fromJust . T.mkSampleRate)
    . getIntProperty c_taglib_properties_samplerate

-- | Get the number of channels.
getChannels :: FileId -> IO T.Channels
getChannels =
  fmap (fromJust . T.mkChannels)
    . getIntProperty c_taglib_properties_channels

----------------------------------------------------------------------------
-- Special convenience ID3v2 functions

-- | Set the default encoding for ID3v2 frames.
id3v2SetEncoding :: T.ID3v2Encoding -> IO ()
id3v2SetEncoding = c_taglib_id3v2_set_default_text_encoding . enumToCInt

----------------------------------------------------------------------------
-- Helpers

getStrValue ::
  -- | How to get a string from the resource
  (Ptr TagLibTag -> IO CString) ->
  -- | File ID
  FileId ->
  -- | String result
  IO Text
getStrValue getStr (FileId ptr) = do
  tag <- c_taglib_file_tag ptr
  cstr <- getStr tag
  result <- packCString cstr
  free cstr
  return (decodeUtf8 result)

getIntValue ::
  (Integral a) =>
  -- | How to get a value from the resource
  (Ptr TagLibTag -> IO a) ->
  -- | File ID
  FileId ->
  -- | Result value
  IO Int
getIntValue getInt (FileId ptr) = do
  tag <- c_taglib_file_tag ptr
  cint <- getInt tag
  return (fromIntegral cint)

setStrValue ::
  -- | Setting routine
  (Ptr TagLibTag -> CString -> IO ()) ->
  -- | New string value
  Text ->
  -- | File ID
  FileId ->
  IO ()
setStrValue setStr str (FileId ptr) = do
  tag <- c_taglib_file_tag ptr
  useAsCString (encodeUtf8 str) $ \cstr ->
    setStr tag cstr

setIntValue ::
  (Integral a) =>
  -- | Setting routine
  (Ptr TagLibTag -> a -> IO ()) ->
  -- | New value
  Maybe Int ->
  -- | File ID
  FileId ->
  IO ()
setIntValue setUInt int (FileId ptr) = do
  tag <- c_taglib_file_tag ptr
  setUInt tag (maybe 0 fromIntegral int)

getIntProperty ::
  (Integral a) =>
  -- | How to get a value from the resource
  (Ptr TagLibProperties -> IO a) ->
  -- | File ID
  FileId ->
  -- | Result
  IO Int
getIntProperty getInt (FileId ptr) = do
  properties <- c_taglib_file_properties ptr
  value <- getInt properties
  return (fromIntegral value)

-- | Convert a Haskell enumeration to a C enumeration (an integer).
enumToCInt :: (Enum a) => a -> CInt
enumToCInt = fromIntegral . fromEnum
