{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Prim.Memory.PVar.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.PVar.Unsafe
  ( PVar(..)
  -- * Creation
  , newRawPVar
  , newRawPinnedPVar
  , newRawAlignedPinnedPVar
  -- * Conversion
  , toPtrPVar
  , unsafeToPtrPVar
  , unsafeToForeignPtrPVar
  -- * Reset
  , zeroPVar
  -- * Unpacked opartions
  , sizeOfPVar#
  , alignmentPVar#
  , setPVar#
  )
  where

import Control.Prim.Monad
import Data.Prim.Memory.PVar.Internal
import Data.Prim
import Foreign.Prim
import GHC.ForeignPtr


-- | Convert `PVar` into a `ForeignPtr`, very unsafe if not backed by pinned memory.
--
-- @since 0.1.0
unsafeToForeignPtrPVar :: PVar a s -> ForeignPtr a
unsafeToForeignPtrPVar pvar@(PVar mba#) =
  case unsafeToPtrPVar pvar of
    Ptr addr# -> ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#))
{-# INLINE unsafeToForeignPtrPVar #-}



-- | Extract the address to the mutable variable, but only if it is backed by pinned
-- memory. It is unsafe because even for pinned memory memory can be deallocated if
-- associated `PVar` goes out of scope. Use `Data.Primitive.PVar.withPtrPVar` or
-- `Data.Primitive.PVar.toForeignPtr` instead.
--
-- @since 0.1.0
toPtrPVar :: PVar a s -> Maybe (Ptr a)
toPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toPtrPVar #-}

-- | Fill the contents of mutable variable with byte @c@
--
-- @since 0.1.0
setPVar# ::
     (MonadPrim s m, Prim a)
  => PVar a s
  -> Int# -- ^ Byte value to fill the `PVar` with
  -> m ()
setPVar# pvar@(PVar mba#) a# =
  prim_ (setByteArray# mba# 0# (sizeOfPVar# pvar) a#)
{-# INLINE setPVar# #-}

-- | Reset contents of a mutable variable to zero.
--
-- @since 0.1.0
zeroPVar :: (MonadPrim s m, Prim a) => PVar a s -> m ()
zeroPVar pvar = setPVar# pvar 0#
{-# INLINE zeroPVar #-}

