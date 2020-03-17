{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Primitive.PVar.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar.Unsafe
  ( PVar(..)
  , toPtrPVar
  , unsafeToPtrPVar
  , zeroPVar
  -- * Unpacked opartions
  , sizeOfPVar#
  , alignmentPVar#
  , setPVar#
  -- ** Atomic operations
  , atomicModifyIntArray#
  , atomicModifyIntArray_#
  -- * Interoperability with `ByteArray`
  , copyFromByteArrayPVar
  , copyFromMutableByteArrayPVar
  , copyPVarToMutableByteArray
  -- * Helpers
  , showsType
  , unI#
  )
  where

import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_)
import Data.Primitive.PVar.Internal
import Data.Primitive.ByteArray
import Data.Primitive.Types
import qualified Foreign.Storable as S
import GHC.Exts as Exts
import GHC.ForeignPtr
import Data.Typeable


-- | Get the address to the contents. This is highly unsafe, espcially if memory is not pinned
--
-- @since 0.1.0
unsafeToPtrPVar :: PVar s a -> Ptr a
unsafeToPtrPVar (PVar mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE unsafeToPtrPVar #-}


-- | Extract the address to the mutable variable, but only if it is backed by pinned
-- memory. It is unsafe because even for pinned memory memory can be deallocated if
-- associated `PVar` goes out of scope. Use `Data.Primitive.PVar.withPtrPVar` or
-- `Data.Primitive.PVar.toForeignPtr` instead.
--
-- @since 0.1.0
toPtrPVar :: PVar s a -> Maybe (Ptr a)
toPtrPVar pvar@(PVar mba#)
  | isTrue# (isMutableByteArrayPinned# mba#) = Just $ unsafeToPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toPtrPVar #-}

-- | Fill the contents of mutable variable with byte @c@
--
-- @since 0.1.0
setPVar# ::
     (PrimMonad m, Prim a)
  => PVar (PrimState m) a
  -> Int# -- ^ Byte value to fill the `PVar` with
  -> m ()
setPVar# pvar@(PVar mba#) a# =
  primitive_ (Exts.setByteArray# mba# 0# (sizeOfPVar# pvar) a#)
{-# INLINE setPVar# #-}

-- | Reset contents of a mutable variable to zero.
--
-- @since 0.1.0
zeroPVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> m ()
zeroPVar pvar = setPVar# pvar 0#
{-# INLINE zeroPVar #-}

-- | Copy the value from a mutable variable into a mutable array at the specified index. Index
-- of array is not checked and can result in an unchecked exception when incorrect
--
-- @since 0.1.0
copyPVarToMutableByteArray ::
     (PrimMonad m, Prim a)
  => PVar (PrimState m) a
  -> MutableByteArray (PrimState m)
  -> Int -- ^ Offset in number of elements into the array
  -> m ()
copyPVarToMutableByteArray pvar mba offset =
  copyBytesPVarToMutableByteArray pvar mba (offset * sizeOfPVar pvar)
{-# INLINE copyPVarToMutableByteArray #-}


-- | Copy the value from a frozen `ByteArray` into a mutable variable at specified
-- index. Index of array is not checked and can result in an unchecked exception when
-- incorrect
--
-- @since 0.1.0
copyFromByteArrayPVar ::
     (PrimMonad m, Prim a)
  => ByteArray -- ^ Source array
  -> Int -- ^ Offset in number of elements into the array
  -> PVar (PrimState m) a
  -> m ()
copyFromByteArrayPVar ba offset pvar =
  copyBytesFromByteArrayPVar ba (offset * sizeOfPVar pvar) pvar
{-# INLINE copyFromByteArrayPVar #-}

-- | Copy the value from MutableByteArray at specified index into the mutable
-- variable. Index of array is not checked and can result in an unchecked exception when
-- incorrect
--
-- @since 0.1.0
copyFromMutableByteArrayPVar ::
     (PrimMonad m, Prim a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ Offset in number of elements into the array
  -> PVar (PrimState m) a
  -> m ()
copyFromMutableByteArrayPVar mba offset pvar =
  copyBytesFromMutableByteArrayPVar mba (offset * sizeOfPVar pvar) pvar
{-# INLINE copyFromMutableByteArrayPVar #-}


-- | Copy the value from a mutable variable into a `MutableByteArray` at the specified
-- offset in number of bytes. Offset into the array is not checked and can result in an
-- unchecked exception when incorrect
--
-- @since 0.1.0
copyBytesPVarToMutableByteArray ::
     (PrimMonad m, Prim a)
  => PVar (PrimState m) a
  -> MutableByteArray (PrimState m)
  -> Int -- ^ Offset in bytes into the array
  -> m ()
copyBytesPVarToMutableByteArray pvar@(PVar mbas#) (MutableByteArray mbad#) (I# offset#) =
  primitive_ (copyMutableByteArray# mbas# 0# mbad# offset# (sizeOfPVar# pvar))
{-# INLINE copyBytesPVarToMutableByteArray #-}


-- | Copy the value from a frozen `ByteArray` at the specified offset in number of bytes
-- into a mutable variable. Offset into the array is not checked and can result in an
-- unchecked exception when incorrect
--
-- @since 0.1.0
copyBytesFromByteArrayPVar ::
     (PrimMonad m, Prim a)
  => ByteArray -- ^ Source array
  -> Int -- ^ Offset in bytes into the array
  -> PVar (PrimState m) a
  -> m ()
copyBytesFromByteArrayPVar (ByteArray ba#) (I# offset#) pvar@(PVar mba#) =
  primitive_ (copyByteArray# ba# offset# mba# 0# (sizeOfPVar# pvar))
{-# INLINE copyBytesFromByteArrayPVar #-}

-- | Copy the value from a `MutableByteArray` at an offset in bytes into the mutable
-- variable. Offset into the array is not checked and can result in an unchecked exception
-- when incorrect
--
-- @since 0.1.0
copyBytesFromMutableByteArrayPVar ::
     (PrimMonad m, Prim a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ Offset in bytes into the array
  -> PVar (PrimState m) a
  -> m ()
copyBytesFromMutableByteArrayPVar (MutableByteArray mbas#) (I# offset#) pvar@(PVar mbad#) =
  primitive_ (copyMutableByteArray# mbas# offset# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyBytesFromMutableByteArrayPVar #-}


showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep
