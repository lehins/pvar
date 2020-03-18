# Changelog for pvar

## 0.1.1.0

* Addition of backwards compatible:
  * `isByteArrayPinned`, `isMutableByteArrayPinned` (that work on ghc-7.10 and ghc-8.0)
  * Primitive versions `isByteArrayPinned#`, `isMutableByteArrayPinned#`
* Support for GHC 7.10 and GHC 8.0
* Re-export `sizeOf` and `alignment` for easier compatibility with older primitive versions.


## 0.1.0.0

* Initial release
