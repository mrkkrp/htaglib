# HTagLib

*Awaiting its test-suite before going to Hackage/Stackage…*

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/htaglib.svg?style=flat)](https://hackage.haskell.org/package/htaglib)
[![Build Status](https://travis-ci.org/mrkkrp/htaglib.svg?branch=master)](https://travis-ci.org/mrkkrp/htaglib)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/htaglib/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/htaglib?branch=master)

* [Quick start](#quick-start)
    * [Reading meta data](#reading-meta-data)
    * [Writing meta data](#writing-meta-data)
    * [Conclusion](#conclusion)
* [License](#license)

This is Haskell bindings to [TagLib](https://taglib.github.io/), library for
reading and editing meta-data of several popular audio formats. This library
is easy to use and type-safe.

It works with the following formats:

* MP3
* FLAC
* MPC
* Speex
* WavPack TrueAudio
* WAV
* AIFF
* MP4
* ASF

This happens in abstract, uniform way, so you don't need to handle any
low-level details. As a consequence, it's currently not possible to work
with format-specific functionality.

## Quick start

First, since this is bindings to C-interface of the library, you'll need to
install the library itself. If you're on Unix-like system, chances are
you'll have it in official repositories of your distro. Users of other
systems should also be able to install it without particular pain.

After installation of the library, install `htaglib` package using Cabal or
Stack (recommended):

```
$ stack install htaglib
```

### Reading meta data

Now to the hacking. It's recommended that you define a record representing
meta-data of audio track in your program, like this:

```haskell
module Main (main) where

import Data.Monoid
import Sound.HTagLib
import System.Environment (getArgs)

data AudioTrack = AudioTrack
  { atTitle   :: Title
  , atArtist  :: Artist
  , atAlbum   :: Album
  , atComment :: Comment
  , atGenre   :: Genre
  , atYear    :: Maybe Year
  , atTrack   :: Maybe TrackNumber }
  deriving Show
```

A couple of notes here. We use unique types for every component of meta
data, so it's more difficult to use track title in lieu of track artist, for
example. Meta data that is represented by strings also has smart
constructors, they replace zero bytes with spaces, this is necessary to
avoid troubles when your Haskell strings go to C-level (well, zero-bytes in
strings is rather edge case, but it should be mentioned). Of course,
`Title`, `Artist`, `Album`, `Comment`, and `Genre` all are instances of
`IsString`, so just turn on `OverloadedStrings` and you can use normal
string literals to create data of these types.

`Year` and `TrackNumber` may be not set or missing, in this case you get
`Nothing`. This is possible with string-based fields too, but in that case
you just get empty strings. `Year` and `TrackNumber` have smart constructors
that make sure that the values are positive (i.e. zero is not allowed).

OK, it's time to read some info. There is `TagGetter` type which is an
applicative functor. You first construct `TagGetter` which will retrieve
entire `AudioTrack` for you using applicative style:

```haskell
audioTrackGetter :: TagGetter AudioTrack
audioTrackGetter = AudioTrack
  <$> titleGetter
  <*> artistGetter
  <*> albumGetter
  <*> commentGetter
  <*> genreGetter
  <*> yearGetter
  <*> trackNumberGetter
```

Perfect, now use `getTags` to read entire record:

```haskell
main :: IO ()
main = do
  path  <- head <$> getArgs
  track <- getTags path audioTrackGetter
  print track
```

For example (alignment is added):

```
$ ./example "/home/mark/music/David Bowie/1977, Low/01 Speed of Life.flac"
AudioTrack
  { atTitle   = Title   {getTitle   = "Speed of Life"}
  , atArtist  = Artist  {getArtist  = "David Bowie"}
  , atAlbum   = Album   {getAlbum   = "Low"}
  , atComment = Comment {getComment = ""}
  , atGenre   = Genre   {getGenre   = ""}
  , atYear    = Just    (Year {getYear = 1977})
  , atTrack   = Just    (TrackNumber {getTrackNumber = 1})
  }
```

Success! It's also possible to extract audio properties like sample rate,
etc. but it's not shown here for simplicity, consult Haddocks for more
information.

### Writing meta data

We cannot use applicative interface to set tags. There are several reasons:

* Applicative interface in general is better for extracting or parsing (or
  rather assembling complex parsers from more basic ones).

* Some fields like sample rate or length can only be read, not set.

* We may wish to set one or two fields selectively, not everything.

Solution: use monoids. `TagSetter` is an instance of `Monoid`. This means
that we can set title and artist of audio track like this:

```haskell
main :: IO ()
main = do
  (path : title : artist : _) <- getArgs
  setTags path Nothing $
    titleSetter (mkTitle title) <>
    artistSetter (mkArtist artist)
  track <- getTags path audioTrackGetter
  print track
```

This code loads file and changes “title” and “artist” meta data
fields.

## Conclusion

With the interface provided by `getTags` and `setTags` it's not possible to
forget to close file or free some resource. You can read all meta data at
once directly into your data structure in type-safe manner. Writing meta
data should be trivial too. Have fun!

## License

Copyright © 2015 Mark Karpov

Distributed under BSD 3 clause license.
