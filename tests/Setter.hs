-- -*- Mode: Haskell; -*-
--
-- HTagLib tests, testing of Setters.
--
-- Copyright © 2015 Mark Karpov
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Setter (tests) where

import Data.Monoid
import System.Directory (getTemporaryDirectory, copyFile)
import System.FilePath ((</>), takeFileName)

import Test.Framework
import Test.HUnit hiding (Test, path)

import Sound.HTagLib
import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

tests :: Test
tests = testGroup "Setters" $
        fmap (caseWithFile simpleSetter . fst) fileList ++
        fmap (caseWithFile specializedGetter) fileList

dupeFile :: FilePath -> IO FilePath
dupeFile path = do
  newPath <- (</> takeFileName path) <$> getTemporaryDirectory
  copyFile path newPath
  return newPath

sampleSetter :: TagSetter
sampleSetter =
  mempty <>
  titleSetter (mkTitle "title'") <>
  artistSetter (mkArtist "artist'") <>
  albumSetter (mkAlbum "album'") <>
  commentSetter (mkComment "comment'") <>
  genreSetter (mkGenre "genre'") <>
  yearSetter (mkYear 2056) <>
  trackNumberSetter (mkTrackNumber 8)

updateSampleTags :: AudioTags -> AudioTags
updateSampleTags tags = tags
  { atTitle = mkTitle "title'"
  , atArtist = mkArtist "artist'"
  , atAlbum = mkAlbum "album'"
  , atComment = mkComment "comment'"
  , atGenre = mkGenre "genre'"
  , atYear = mkYear 2056
  , atTrackNumber = mkTrackNumber 8 }

simpleSetter :: FilePath -> Assertion
simpleSetter path = do
  dupe <- dupeFile path
  setTags dupe Nothing $ sampleSetter
  tags <- getTags dupe (sampleGetter dupe)
  tags @?= updateSampleTags (sampleTags dupe)

specializedGetter :: (FilePath, FileType) -> Assertion
specializedGetter (path, t) = do
  dupe <- dupeFile path
  setTags' dupe Nothing t $ sampleSetter
  tags <- getTags dupe (sampleGetter dupe)
  tags @?= updateSampleTags (sampleTags dupe)
