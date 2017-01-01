-- |
-- Module      :  Sound.HTagLib
-- Copyright   :  © 2015–2017 Mark Karpov
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
import Sound.HTagLib.Setter
import Sound.HTagLib.Type
