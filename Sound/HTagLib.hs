-- |
-- Module      :  Sound.HTagLib
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes complete high-level interface to TagLib. This is the
-- module you should import to use in your projects.

module Sound.HTagLib
  ( -- * Data types
    Title
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
  , getChannels
  , FileType (..)
  , ID3v2Encoding (..)
  , HTagLibException (..)
    -- * Getters
  , TagGetter
  , getTags
  , getTags'
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
  , channelsGetter
    -- * Setters
  , TagSetter
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

import Sound.HTagLib.Getter
import Sound.HTagLib.Internal
  ( FileType (..)
  , ID3v2Encoding (..)
  , HTagLibException (..) )
import Sound.HTagLib.Setter
import Sound.HTagLib.Type
