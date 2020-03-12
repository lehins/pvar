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
  , setPVar#
  , zeroPVar
  -- * Atomic operations
  , atomicModifyIntArray#
  )
  where

import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_)
import Data.Primitive.PVar.Internal
import Data.Primitive.ByteArray
import Data.Primitive.Types
import qualified Foreign.Storable as S
import GHC.Exts as Exts
import GHC.ForeignPtr

-- | Get the address to the contents. This is highly unsafe if memory is not pinned
unsafeToPtrPVar :: PVar s a -> Ptr a
unsafeToPtrPVar (PVar mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE unsafeToPtrPVar #-}


-- | Extract the address to the mutable variable, but only if it is backed by pinned
-- memory
toPtrPVar :: PVar s a -> Maybe (Ptr a)
toPtrPVar (PVar mba#)
  | isTrue# (isMutableByteArrayPinned# mba#) =
    Just (Ptr (byteArrayContents# (unsafeCoerce# mba#)))
  | otherwise = Nothing
{-# INLINE toPtrPVar #-}

-- | Fill the contents of mutable variable with byte @c@
setPVar# ::
     (PrimMonad m, Prim a)
  => PVar (PrimState m) a
  -> Int# -- ^ Byte value to fill the `PVar` with
  -> m ()
setPVar# pvar@(PVar mba#) a# =
  primitive_ (Exts.setByteArray# mba# 0# a# (sizeOfPVar# pvar))
{-# INLINE setPVar# #-}

-- | Reset contents of a mutable variable to zero.
zeroPVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> m ()
zeroPVar pvar = setPVar# pvar 0#
{-# INLINE zeroPVar #-}


copyPVarToMutableByteArray ::
     (PrimMonad m, Prim a)
  => PVar (PrimState m) a
  -> MutableByteArray (PrimState m)
  -> Int -- ^ Offset in bytes into the array
  -> m ()
copyPVarToMutableByteArray pvar@(PVar mbas#) (MutableByteArray mbad#) (I# offset#) =
  primitive_ (copyMutableByteArray# mbas# 0# mbad# offset# (sizeOfPVar# pvar))
{-# INLINE copyPVarToMutableByteArray #-}


copyFromByteArrayPVar ::
     (PrimMonad m, Prim a)
  => ByteArray -- ^ Source array
  -> Int -- ^ Offset in bytes into the array
  -> PVar (PrimState m) a
  -> m ()
copyFromByteArrayPVar (ByteArray ba#) (I# offset#) pvar@(PVar mba#) =
  primitive_ (copyByteArray# ba# offset# mba# 0# (sizeOfPVar# pvar))
{-# INLINE copyFromByteArrayPVar #-}

-- | Copy a value from MutableByteArray at an offset to the mutable variable.
copyFromMutableByteArrayPVar ::
     (PrimMonad m, Prim a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ Offset in bytes into the array
  -> PVar (PrimState m) a
  -> m ()
copyFromMutableByteArrayPVar (MutableByteArray mbas#) (I# offset#) pvar@(PVar mbad#) =
  primitive_ (copyMutableByteArray# mbas# offset# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyFromMutableByteArrayPVar #-}


