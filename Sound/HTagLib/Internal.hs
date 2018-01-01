-- |
-- Module      :  Sound.HTagLib.Internal
-- Copyright   :  © 2015–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level interaction with underlying C API. You don't want to use this,
-- see "Sound.HTagLib" instead.

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.HTagLib.Internal
  ( -- * Data types
    FileId
    -- * File API
  , withFile
  , saveFile
    -- * Tag API
  , getTitle
  , getArtist
  , getAlbum
  , getComment
  , getGenre
  , getYear
  , getTrackNumber
  , setTitle
  , setArtist
  , setAlbum
  , setComment
  , setGenre
  , setYear
  , setTrackNumber
    -- * Audio properties API
  , getDuration
  , getBitRate
  , getSampleRate
  , getChannels
    -- * Special convenience ID3v2 functions
  , id3v2SetEncoding )
where

import Control.Exception (throw, bracket)
import Control.Monad (when, unless)
import Data.ByteString (packCString, useAsCString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Sound.HTagLib.Type as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

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

-- | Open audio file and return its ID (an opaque type that the rest of
-- library can pass around). In case of trouble 'IOException' is thrown.

newFile
  :: FilePath          -- ^ Path to audio file
  -> Maybe T.FileType  -- ^ Type of file (or it will be guessed)
  -> IO FileId         -- ^ Id to pass around
newFile path ftype = do
  c_taglib_set_string_management_enabled $ fromBool False
  ptr <- withCString path $ \cstr ->
    case ftype of
      Nothing -> c_taglib_file_new cstr
      Just t  -> c_taglib_file_new_type cstr (enumToCInt t)
  when (ptr == nullPtr) $
    throw (T.OpeningFailed path)
  valid <- toBool <$> c_taglib_file_is_valid ptr
  unless valid $
    throw (T.InvalidFile path)
  return (FileId ptr)

-- | Free file given its ID. Every time you open a file, free it.

freeFile :: FileId -> IO ()
freeFile (FileId ptr) = c_taglib_file_free ptr

-- | Open audio file located at specified path, execute some actions given
-- its 'FileId' and then free the file.

withFile
  :: FilePath          -- ^ Path to audio file
  -> Maybe T.FileType  -- ^ Type of file (or it will be guessed)
  -> (FileId -> IO a)  -- ^ Computation depending of 'FileId'
  -> IO a              -- ^ Result value
withFile path t = bracket (newFile path t) freeFile

-- | Save file given its ID. Given 'FilePath' just tells what to put into
-- exception if the action fails, it doesn't specify where to save the file
-- (it's determined by 'FileId').

saveFile
  :: FilePath          -- ^ File name to use in exceptions
  -> FileId            -- ^ File identifier
  -> IO ()
saveFile path (FileId ptr) = do
  success <- toBool <$> c_taglib_file_save ptr
  unless success $
    throw (T.SavingFailed path)

----------------------------------------------------------------------------
-- Tag API

-- | Get title tag associated with file.

getTitle :: FileId -> IO T.Title
getTitle = fmap T.mkTitle . getStrValue c_taglib_tag_title

-- | Get artist tag associated with file.

getArtist :: FileId -> IO T.Artist
getArtist = fmap T.mkArtist . getStrValue c_taglib_tag_artist

-- | Get album tag associated with file.

getAlbum :: FileId -> IO T.Album
getAlbum = fmap T.mkAlbum . getStrValue c_taglib_tag_album

-- | Get comment tag associated with file.

getComment :: FileId -> IO T.Comment
getComment = fmap T.mkComment . getStrValue c_taglib_tag_comment

-- | Get genre tag associated with file.

getGenre :: FileId -> IO T.Genre
getGenre = fmap T.mkGenre . getStrValue c_taglib_tag_genre

-- | Get year tag associated with file.

getYear :: FileId -> IO (Maybe T.Year)
getYear = fmap T.mkYear . getIntValue c_taglib_tag_year

-- | Get track number associated with file.

getTrackNumber :: FileId -> IO (Maybe T.TrackNumber)
getTrackNumber = fmap T.mkTrackNumber . getIntValue c_taglib_tag_track

-- | Set title of track associated with file.

setTitle :: T.Title -> FileId -> IO ()
setTitle v = setStrValue c_taglib_tag_set_title (T.unTitle v)

-- | Set artist of track associated with file.

setArtist :: T.Artist -> FileId -> IO ()
setArtist v = setStrValue c_taglib_tag_set_artist (T.unArtist v)

-- | Set album of track associated with file.

setAlbum :: T.Album -> FileId -> IO ()
setAlbum v = setStrValue c_taglib_tag_set_album (T.unAlbum v)

-- | Set comment of track associated with file.

setComment :: T.Comment -> FileId -> IO ()
setComment v = setStrValue c_taglib_tag_set_comment (T.unComment v)

-- | Set genre of track associated with file.

setGenre :: T.Genre -> FileId -> IO ()
setGenre v = setStrValue c_taglib_tag_set_genre (T.unGenre v)

-- | Set year of track associated with file.

setYear :: Maybe T.Year -> FileId -> IO ()
setYear v = setIntValue c_taglib_tag_set_year (T.unYear <$> v)

-- | Set track number of track associated with file.

setTrackNumber :: Maybe T.TrackNumber -> FileId -> IO ()
setTrackNumber v = setIntValue c_taglib_tag_set_track (T.unTrackNumber <$> v)

----------------------------------------------------------------------------
-- Audio properties API

-- | Get duration of track associated with file.

getDuration :: FileId -> IO T.Duration
getDuration = fmap (fromJust . T.mkDuration)
  . getIntProperty c_taglib_properties_length

-- | Get bit rate of track associated with file.

getBitRate :: FileId -> IO T.BitRate
getBitRate = fmap (fromJust . T.mkBitRate)
  . getIntProperty c_taglib_properties_bitrate

-- | Get sample rate of track associated with file.

getSampleRate :: FileId -> IO T.SampleRate
getSampleRate = fmap (fromJust . T.mkSampleRate)
  . getIntProperty c_taglib_properties_samplerate

-- | Get number of channels in track associated with file.

getChannels :: FileId -> IO T.Channels
getChannels = fmap (fromJust . T.mkChannels)
  . getIntProperty c_taglib_properties_channels

----------------------------------------------------------------------------
-- Special convenience ID3v2 functions

-- | Set the default encoding for ID3v2 frames that are written to tags.

id3v2SetEncoding :: T.ID3v2Encoding -> IO ()
id3v2SetEncoding = c_taglib_id3v2_set_default_text_encoding . enumToCInt

----------------------------------------------------------------------------
-- Helpers

getStrValue
  :: (Ptr TagLibTag -> IO CString) -- ^ How to get string from the resource
  -> FileId            -- ^ File ID
  -> IO Text           -- ^ String result
getStrValue getStr (FileId ptr) = do
  tag    <- c_taglib_file_tag ptr
  cstr   <- getStr tag
  result <- packCString cstr
  free cstr
  return (decodeUtf8 result)

getIntValue :: Integral a
  => (Ptr TagLibTag -> IO a) -- ^ How to get value from the resource
  -> FileId            -- ^ File ID
  -> IO Int            -- ^ Result value
getIntValue getInt (FileId ptr) = do
  tag  <- c_taglib_file_tag ptr
  cint <- getInt tag
  return (fromIntegral cint)

setStrValue
  :: (Ptr TagLibTag -> CString -> IO ()) -- ^ Setting routine
  -> Text              -- ^ New string value
  -> FileId            -- ^ File ID
  -> IO ()
setStrValue setStr str (FileId ptr) = do
  tag <- c_taglib_file_tag ptr
  useAsCString (encodeUtf8 str) $ \cstr ->
    setStr tag cstr

setIntValue :: Integral a
  => (Ptr TagLibTag -> a -> IO ()) -- ^ Setting routine
  -> Maybe Int         -- ^ New value
  -> FileId            -- ^ File ID
  -> IO ()
setIntValue setUInt int (FileId ptr) = do
  tag <- c_taglib_file_tag ptr
  setUInt tag (maybe 0 fromIntegral int)

getIntProperty :: Integral a
  => (Ptr TagLibProperties -> IO a) -- ^ How to get value from the resource
  -> FileId            -- ^ File ID
  -> IO Int            -- ^ Result
getIntProperty getInt (FileId ptr) = do
  properties <- c_taglib_file_properties ptr
  value      <- getInt properties
  return (fromIntegral value)

-- | Convert Haskell enumeration to C enumeration (an integer).

enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum
