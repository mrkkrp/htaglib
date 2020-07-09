{-# LANGUAGE OverloadedStrings #-}

module Sound.HTagLib.SetterSpec (spec) where

import Sound.HTagLib
import Sound.HTagLib.Test.Util
import System.Directory (copyFile, getTemporaryDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Hspec

spec :: Spec
spec =
  describe "setters" $ do
    mapM_ (withFile $ const simpleSetter) fileList
    mapM_ (withFile specializedSetter) fileList

dupeFile :: FilePath -> IO FilePath
dupeFile path = do
  newPath <- (</> takeFileName path) <$> getTemporaryDirectory
  copyFile path newPath
  return newPath

updateSampleTags :: AudioTags -> AudioTags
updateSampleTags tags =
  tags
    { atTitle = mkTitle "title'",
      atArtist = mkArtist "artist'",
      atAlbum = mkAlbum "album'",
      atComment = mkComment "comment'",
      atGenre = mkGenre "genre'",
      atYear = mkYear 2056,
      atTrackNumber = mkTrackNumber 8
    }

simpleSetter :: AudioTags -> Expectation
simpleSetter tags = do
  let path = atFileName tags
  dupe <- dupeFile path
  setTags dupe Nothing sampleSetter
  extracted <- getTags dupe (sampleGetter dupe)
  extracted `shouldMatchTags` updateSampleTags (tags {atFileName = dupe})

specializedSetter :: FileType -> AudioTags -> Expectation
specializedSetter t tags = do
  let path = atFileName tags
  dupe <- dupeFile path
  setTags' dupe Nothing t sampleSetter
  extracted <- getTags dupe (sampleGetter dupe)
  extracted `shouldMatchTags` updateSampleTags (tags {atFileName = dupe})
