{-# LANGUAGE OverloadedStrings #-}

module Sound.HTagLib.Test.Util
  ( AudioTags (..),
    sampleGetter,
    sampleSetter,
    fileList,
    withFile,
    shouldMatchTags,
  )
where

import Data.Maybe (fromJust)
import Sound.HTagLib
import Test.Hspec

data AudioTags = AudioTags
  { atFileName :: FilePath,
    atTitle :: Title,
    atArtist :: Artist,
    atAlbum :: Album,
    atComment :: Comment,
    atGenre :: Genre,
    atYear :: Maybe Year,
    atTrackNumber :: Maybe TrackNumber,
    atDuration :: Duration,
    atBitRate :: BitRate,
    atSampleRate :: SampleRate,
    atChannels :: Channels
  }
  deriving (Show, Eq)

sampleGetter :: FilePath -> TagGetter AudioTags
sampleGetter path =
  AudioTags
    <$> pure path
    <*> titleGetter
    <*> artistGetter
    <*> albumGetter
    <*> commentGetter
    <*> genreGetter
    <*> yearGetter
    <*> trackNumberGetter
    <*> durationGetter
    <*> bitRateGetter
    <*> sampleRateGetter
    <*> channelsGetter

sampleSetter :: TagSetter
sampleSetter =
  mempty
    <> titleSetter (mkTitle "title'")
    <> artistSetter (mkArtist "artist'")
    <> albumSetter (mkAlbum "album'")
    <> commentSetter (mkComment "comment'")
    <> genreSetter (mkGenre "genre'")
    <> yearSetter (mkYear 2056)
    <> trackNumberSetter (mkTrackNumber 8)

sampleTags :: AudioTags
sampleTags =
  AudioTags
    { atFileName = undefined,
      atTitle = "title",
      atArtist = "artist",
      atAlbum = "album",
      atComment = "comment",
      atGenre = "genre",
      atYear = mkYear 2055,
      atTrackNumber = mkTrackNumber 7,
      atDuration = fromJust $ mkDuration 0,
      atBitRate = fromJust $ mkBitRate 0,
      atSampleRate = fromJust $ mkSampleRate 44100,
      atChannels = fromJust $ mkChannels 2
    }

fileList :: [(FileType, AudioTags)]
fileList =
  [ ( FLAC,
      sampleTags
        { atBitRate = fromJust $ mkBitRate 217,
          atFileName = "audio-samples/sample.flac"
        }
    ),
    ( MPEG,
      sampleTags
        { atBitRate = fromJust $ mkBitRate 136,
          atFileName = "audio-samples/sample.mp3"
        }
    )
  ]

-- | Call given function with provided data.
withFile ::
  (FileType -> AudioTags -> Expectation) ->
  (FileType, AudioTags) ->
  Spec
withFile f (t, tags) = it name (f t tags)
  where
    name = "using file: " ++ show (atFileName tags) ++ " (" ++ show t ++ ")"

-- | Create an expectation that two collections of tags match. However, if
-- bit rate of the first is zero (which is the case with older versions of
-- TagLib when it's used with such short files as our samples), allow bit
-- rate values differ.
shouldMatchTags ::
  -- | Tags to test
  AudioTags ->
  -- | Correct tags to test against
  AudioTags ->
  Expectation
shouldMatchTags given expected =
  let zeroBitRate = fromJust (mkBitRate 0)
   in if atBitRate given == zeroBitRate
        then given `shouldBe` expected {atBitRate = zeroBitRate}
        else given `shouldBe` expected
