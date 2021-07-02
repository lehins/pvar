{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module      : Data.Prim.PVar.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.PVar.Internal
  ( PVar(..)
  , newPVar
  , newPinnedPVar
  , newAlignedPinnedPVar
  , newRawPVar
  , newRawPinnedPVar
  , newRawAlignedPinnedPVar
  , unsafeToPtrPVar
  , unsafeWithPtrPVar
  , readPVar
  , writePVar
  , isPinnedPVar
  , sizeOfPVar
  , sizeOfPVar#
  , alignmentPVar
  , alignmentPVar#
  )
  where

import Control.DeepSeq
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Class
import Foreign.Prim

-- | Mutable variable with primitive value.
--
-- @since 0.1.0
data PVar a s = PVar (MutableByteArray# s)

-- | Values are already written into `PVar` in NF, this instance is trivial.
instance NFData (PVar a s) where
  rnf (PVar _) = ()

-- | Create a mutable variable in unpinned memory (i.e. GC can move it) with an initial
-- value. This is a prefered way to create a mutable variable, since it will not
-- contribute to memory fragmentation. For pinned memory versions see `newPinnedPVar` and
-- `newAlignedPinnedPVar`
--
-- @since 0.1.0
newPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newPVar v = do
  pvar <- newRawPVar
  pvar <$ writePVar pvar v
{-# INLINE newPVar #-}

-- | Create a mutable variable in unpinned and unititialized memory
--
-- @since 0.1.0
newRawPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
newRawPVar =
  prim $ \s# ->
    case newByteArray# (unCountBytes# (1 :: Count a)) s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE newRawPVar #-}


-- | Create a mutable variable in pinned memory with an initial value.
--
-- @since 0.1.0
newPinnedPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newPinnedPVar v = do
  pvar <- newRawPinnedPVar
  writePVar pvar v
  return pvar
{-# INLINE newPinnedPVar #-}

-- | Create a mutable variable in pinned memory with uninitialized memory.
--
-- @since 2.0.0
newRawPinnedPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
newRawPinnedPVar =
  prim $ \s# ->
    case newPinnedByteArray# (unCountBytes# (1 :: Count a)) s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE newRawPinnedPVar #-}


-- | Create a mutable variable in pinned memory with an initial value and aligned
-- according to its `Data.Primitive.Types.alignment`
--
-- @since 0.1.0
newAlignedPinnedPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newAlignedPinnedPVar v = do
  pvar <- newRawAlignedPinnedPVar
  writePVar pvar v
  return pvar
{-# INLINE newAlignedPinnedPVar #-}


-- | Create a mutable variable in pinned uninitialized memory.
--
-- @since 2.0.0
newRawAlignedPinnedPVar ::
     forall e m s. (MonadPrim s m, Prim e)
  => m (PVar e s)
newRawAlignedPinnedPVar =
  prim $ \s# ->
    case newAlignedPinnedByteArray#
           (unCountBytes# (1 :: Count e))
           (alignment# (proxy# :: Proxy# e))
           s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE newRawAlignedPinnedPVar #-}


-- | Get the address to the contents. This is highly unsafe, espcially if memory is not pinned
--
-- @since 0.1.0
unsafeToPtrPVar :: PVar a s -> Ptr a
unsafeToPtrPVar (PVar mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE unsafeToPtrPVar #-}

-- helper that fills the PVar before running the action
unsafeWithPtrPVar ::
     (Prim a, MonadPrim s m)
  => PVar a s
  -> (PVar a s -> Ptr a -> m b)
  -> m b
unsafeWithPtrPVar pvar f = do
  let ptr = unsafeToPtrPVar pvar
  r <- f pvar ptr
  r <$ touch pvar
{-# INLINE unsafeWithPtrPVar #-}


-- | Read a value from a mutable variable
--
-- @since 0.1.0
readPVar :: (MonadPrim s m, Prim a) => PVar a s -> m a
readPVar (PVar mba#) = prim (readMutableByteArray# mba# 0#)
{-# INLINE readPVar #-}

-- | Write a value into a mutable variable
--
-- @since 0.1.0
writePVar :: (MonadPrim s m, Prim a) => PVar a s -> a -> m ()
writePVar (PVar mba#) v = prim_ (writeMutableByteArray# mba# 0# v)
{-# INLINE writePVar #-}

-- | Get the size of the value stored in a mutable variable in bytes as an unpacked
-- integer
--
-- @since 0.1.0
sizeOfPVar# :: forall a s. Prim a => PVar a s -> Int#
sizeOfPVar# _ = sizeOf# (proxy# :: Proxy# a)
{-# INLINE sizeOfPVar# #-}

-- | Get the alignment of the value stored in a mutable variable in bytes as an unpacked
-- integer
--
-- @since 0.1.0
alignmentPVar# :: forall a s. Prim a => PVar a s -> Int#
alignmentPVar# _ = alignment# (proxy# :: Proxy# a)
{-# INLINE alignmentPVar# #-}


-- | Size in bytes of a value stored inside the mutable variable. `PVar` itself is neither
-- accessed nor evaluated.
--
-- @since 0.1.0
sizeOfPVar :: Prim a => PVar a s -> Int
sizeOfPVar pvar = I# (sizeOfPVar# pvar)
{-# INLINE sizeOfPVar #-}

-- | Alignment in bytes of the value stored inside of the mutable variable. `PVar` itself is
-- neither accessed nor evaluated.
--
-- @since 0.1.0
alignmentPVar :: Prim a => PVar a s -> Int
alignmentPVar pvar = I# (alignmentPVar# pvar)
{-# INLINE alignmentPVar #-}


-- | Check if `PVar` is backed by pinned memory or not
--
-- @since 0.1.0
isPinnedPVar :: PVar a s -> Bool
isPinnedPVar (PVar mba#) = isTrue# (isMutableByteArrayPinned# mba#)
{-# INLINE isPinnedPVar #-}

