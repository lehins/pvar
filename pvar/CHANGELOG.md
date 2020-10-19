# Changelog for pvar

## 0.2.0.0

* Switch from `primitive` to `primal`
* Remove all `Storable` related functionality, since `Prim` class supercedes it.
* Remove compatibility with primops and remove `ByteArray` related functionality.
* Copy and move functions will be found in `primal-memory` package
* Atomic operations are now polymorphic that work on `Atomic` instances, instead of being
  restricted to `Int` only.
* Modify functions have been renamed for consistency and to better portray the intent
  (i.e. returning old vs new value)

## 1.0.0.0

* Switch to `MonadPrim` type classes that was introduced in `primitive-0.7.1.0`
* Fix definition `PVar m a` -> `PVar a s`. Parametrization on a monad is not general
  enough for some cases and making state token `s` last allows to use it with `forall`
  easier.
* Relax monad in conversion to ForeignPtr

## 0.2.0.0

* Rename `modifyPVar` to `fetchModifyPVar` and `modifyPVarM` to `fetchModifyPVarM`. This
  is a breaking change in favor of consistency with other librarries.
* New implementation for `modifyPVar` and `modifyPVarM` that can return some artifact.
* Addition of `modifyFetchPVar` and `modifyFetchPVarM`
* Addition of `atomicModifyFetchIntPVar` and `atomicFetchModifyIntPVar`

## 0.1.1.0

* Addition of backwards compatible:
  * `isByteArrayPinned`, `isMutableByteArrayPinned` (that work on ghc-7.10 and ghc-8.0)
  * Primitive versions `isByteArrayPinned#`, `isMutableByteArrayPinned#`
* Support for GHC 7.10 and GHC 8.0
* Re-export `sizeOf` and `alignment` for easier compatibility with older primitive versions.


## 0.1.0.0

* Initial release
