# HTagLib

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/htaglib.svg?style=flat)](https://hackage.haskell.org/package/htaglib)
[![Stackage Nightly](http://stackage.org/package/htaglib/badge/nightly)](http://stackage.org/nightly/package/htaglib)
[![Stackage LTS](http://stackage.org/package/htaglib/badge/lts)](http://stackage.org/lts/package/htaglib)
![CI](https://github.com/mrkkrp/htaglib/workflows/CI/badge.svg?branch=master)

* [Alternatives](#alternatives)
* [A note for FLAC users](#a-note-for-flac-users)
* [Quick start](#quick-start)
    * [Reading meta data](#reading-meta-data)
    * [Writing meta data](#writing-meta-data)
    * [Conclusion](#conclusion)
* [License](#license)

This is Haskell bindings to [TagLib](https://taglib.github.io/), library for
reading and editing meta-data of several popular audio formats.

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

This happens in an abstract, uniform way, so you don't need to handle any
low-level details. As a consequence, it's currently not possible to work
with format-specific functionality.

## Alternatives

There is at least two Haskell bindings doing “the same” thing:

* [`libtagc`](https://hackage.haskell.org/package/libtagc)
* [`taglib`](https://hackage.haskell.org/package/taglib)

Both are very low level, without any protection or higher-level
abstractions, not really type-safe. I personally don't want to use them, so
I wrote this.

## A note for FLAC users

If you want to work with FLAC, there is a [complete Haskell
binding](https://github.com/mrkkrp/flac) to `libFLAC`—reference FLAC
implementation. It allows to work with all FLAC metadata (read and write)
and also provides Haskell API to stream encoder and stream decoder. Please
prefer that package if you don't need to work with other audio formats.

## Quick start

First, since this is bindings to C-interface of the library, you'll need to
install the library itself. If you're on a Unix-like system, chances are
you'll have it in the official repositories of your distro.

### Reading meta data

Now to the hacking. It's recommended that you define a record representing
meta-data of audio track in your program, like this:

```haskell
module Main (main) where

import Data.Monoid
import Sound.HTagLib
import System.Environment (getArgs)

data AudioTrack = AudioTrack
  { atTitle :: Title,
    atArtist :: Artist,
    atAlbum :: Album,
    atComment :: Comment,
    atGenre :: Genre,
    atYear :: Maybe Year,
    atTrack :: Maybe TrackNumber
  }
  deriving (Show)
```

A couple of notes here. We use unique types for every component of meta
data, so it's more difficult to use track title in lieu of track artist, for
example. Meta data that is represented by strings also has smart
constructors, they replace zero bytes with spaces, this is necessary to
avoid troubles when your Haskell strings go to the C-level (well, zero-bytes
in strings is rather edge case, but it should be mentioned). Of course,
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
audioTrackGetter =
  AudioTrack
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
  path <- head <$> getArgs
  track <- getTags path audioTrackGetter
  print track
```

For example (alignment is added):

```
$ ./example "/home/mark/music/David Bowie/1977, Low/01 Speed of Life.flac"
AudioTrack
  { atTitle = Title "Speed of Life",
    atArtist = Artist "David Bowie",
    atAlbum = Album "Low",
    atComment = Comment "",
    atGenre = Genre "",
    atYear = Just (Year 1977),
    atTrack = Just (TrackNumber 1)
  }
```

Success! It's also possible to extract audio properties like sample rate,
etc. but it's not shown here for simplicity, consult Haddocks for more
information.

N.B. If you need to extract duration of tracks, TagLib only returns number
of seconds as an integer. This means that if you want to calculate total
duration, you'll have slightly incorrect result. Proper solution is to
extract duration as floating-point number, for that we recommend bindings to
`libsndfile`—[`hsndfile`](https://hackage.haskell.org/package/hsndfile) (or
the above-mentioned `flac` package for Haskell if you work with FLAC).

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
    titleSetter (mkTitle title)
      <> artistSetter (mkArtist artist)
  track <- getTags path audioTrackGetter
  print track
```

This code loads file and changes “title” and “artist” meta data fields.

## Conclusion

With the interface provided by `getTags` and `setTags` it's not possible to
forget to close file or free some resource. You can read all meta data at
once directly into your data structure in type-safe manner. Writing meta
data should be trivial too. Have fun!

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/htaglib/issues).

Pull requests are also welcome.

## License

Copyright © 2015–present Mark Karpov

Distributed under BSD 3 clause license.
