## HTagLib 1.0.0

* Make the module `Sound.HTagLib.Internal` hidden for end users.

* Make `Text` underlying type for wrappers around textual data (breaking
  change, previously it was `String`).

* Rename functions like `getTitle` to `unTitle` (breaking change).

* Fix bug when wrong data is read from files when current locale specifies
  encoding other than UTF-8.

## HTagLib 0.1.1

* Missing audio samples used for testing are included in distribution.

## HTagLib 0.1.0

* Initial release.
