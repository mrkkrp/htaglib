# HTagLib

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/htaglib.svg?style=flat)](https://hackage.haskell.org/package/htaglib)
[![Build Status](https://travis-ci.org/mrkkrp/htaglib.svg?branch=master)](https://travis-ci.org/mrkkrp/htaglib)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/htaglib/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/htaglib?branch=master)

This is Haskell bindings to [TagLib](https://taglib.github.io/), library for
reading and editing meta-data of several popular audio formats. This library
is easy to use and it's modern Haskell: type-safe, high-level, pleasant to
work with. It's tested and long-term maintained.

These bindings work with the following formats:

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
low-level details. As a consequence, you currently cannot work with
format-specific functionality.

## Quick start

First, since this is bindings to C-interface of the library, you'll need to
install the library itself. If you're on Unix-like system, chances are
you'll have it in your official repositories. Users of other systems should
also be able to install it without particular pain.

After installation of the library, install `htaglib` package using Cabal or
Stack (recommended):

```
$ stack install htaglib
```

Now to the hacking. It's recommended that you define a record representing
meta-data of audio track in your program, like this:

```haskell
import Sound.HTagLib

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

A couple of notes here. We use unique types for every component of
meta-data, so it's more difficult to use track title in lieu of track
artist, for example. Meta-data that is represented by strings also has smart
constructors, they replace zero bytes with spaces, this is necessary to
avoid troubles when your Haskell strings go to C-level. Of course, `Title`,
`Artist`, `Album`, `Comment`, and `Genre` all are instances of `IsString`,
so just turn on `OverloadedStrings` and you can use normal string literals
to create data of these types.

`Year` and `Track` may be not set or missing, in this case you get
`Nothing`. To be honest this is possible with string-based fields too, but
in that case you just get empty strings. `Year` and `Track` have smart
constructors that make sure that the values are positive (i.e. zero is not
allowed).

OK, it's time to read some info. There is `TagGetter` type which is an
applicative functor. You first construct `TagGetter` which will retrive
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

For example:

```
$ ghc --make example.hs

$ ./example.sh ...


```

Success! It's also possible to extract audio properties like sample rate,
etc. but it's not shown here for simplicity, consult Haddocks for more
information.

We cannot use applicative interface to set tags. There are several reasons:

* Applicative interface is general in better for extracting or parsing (or
  rather assembling complex parsers from more basic ones).

* Some fields like sample rate or length can only be read, not set.

* We may which to set one or two fields selectively, not everything.

Solution: use monoids. `TagSetter` is an instance of `Monoid`. This means
that we can set title and artist of audio track like this:

```haskell
main :: IO ()
main = do
  (path : title : artist : _) <- head <$> getArgs
  setTags path (titleSetter title <> artistSetter artist)
  track <- getTags path audioTrackGetter
  print track
```

## License

Copyright © 2015 Mark Karpov

Distributed under BSD 3 clause license.
