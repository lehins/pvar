{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fobject-code #-}
-- |
-- Module      : Data.Primitive.PVar.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar.Internal
  ( PVar(..)
  , newPVar
  , newPinnedPVar
  , newAlignedPinnedPVar
  , readPVar
  , writePVar
  , isPinnedPVar
  , sizeOfPVar
  , sizeOfPVar#
  , alignmentPVar
  , alignmentPVar#
  , unI#
  -- * Atomic operations
  , atomicModifyIntArray#
  , atomicModifyIntPVar
  , atomicModifyIntArray_#
  , atomicModifyIntPVar_
  )
  where

import GHC.Exts
import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_, touch)
import Data.Primitive.Types
import qualified Foreign.Storable as S
import Control.DeepSeq

-- | Mutable variable with primitive value.
--
-- @since 0.1.0
data PVar s a = PVar (MutableByteArray# s)

-- | @`S.poke`+`S.peek`@ will result in a new copy of a `PVar`
instance Prim a => S.Storable (PVar RealWorld a) where
  sizeOf _ = sizeOf (undefined :: a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  {-# INLINE alignment #-}
  peekElemOff (Ptr addr#) (I# i#) = do
    a <- primitive (readOffAddr# addr# i#)
    newAlignedPinnedPVar a
  {-# INLINE peekElemOff #-}
  pokeElemOff (Ptr addr#) (I# i#) pvar = do
    a <- readPVar pvar
    primitive_ (writeOffAddr# addr# i# a)
  {-# INLINE pokeElemOff #-}

-- | Values are already written into `PVar` in NF, this instance is trivial.
instance NFData (PVar s a) where
  rnf (PVar _) = ()

-- | Create a mutable variable in unpinned memory (i.e. GC can move it) with an initial
-- value. This is a prefered way to create a mutable variable, since it will not
-- contribute to memory fragmentation. For pinned memory versions see `newPinnedPVar` and
-- `newAlignedPinnedPVar`
--
-- @since 0.1.0
newPVar :: (PrimMonad m, Prim a) => a -> m (PVar (PrimState m) a)
newPVar v =
  primitive $ \s# ->
    case newByteArray# 1# s# of
      (# s'#, mba# #) -> (# writeByteArray# mba# 0# v s'#, PVar mba# #)
{-# INLINE newPVar #-}

-- | Create a mutable variable in pinned memory with an initial value.
--
-- @since 0.1.0
newPinnedPVar :: (PrimMonad m, Prim a) => a -> m (PVar (PrimState m) a)
newPinnedPVar v =
  primitive $ \s# ->
    case newPinnedByteArray# 1# s# of
      (# s'#, mba# #) -> (# writeByteArray# mba# 0# v s'#, PVar mba# #)
{-# INLINE newPinnedPVar #-}

-- | Create a mutable variable in pinned memory with an initial value and aligned
-- according to its `Data.Primitive.Types.alignment`
--
-- @since 0.1.0
newAlignedPinnedPVar :: (PrimMonad m, Prim a) => a -> m (PVar (PrimState m) a)
newAlignedPinnedPVar v =
  primitive $ \s# ->
    case newAlignedPinnedByteArray# 1# (alignment# v) s# of
      (# s'#, mba# #) -> (# writeByteArray# mba# 0# v s'#, PVar mba# #)
{-# INLINE newAlignedPinnedPVar #-}

-- | Read a value from a mutable variable
--
-- @since 0.1.0
readPVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> m a
readPVar (PVar mba#) = primitive (readByteArray# mba# 0#)
{-# INLINE readPVar #-}

-- | Write a value into a mutable variable
--
-- @since 0.1.0
writePVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> a -> m ()
writePVar (PVar mba#) v = primitive_ (writeByteArray# mba# 0# v)
{-# INLINE writePVar #-}

-- | Get the size of the mutable variable in bytes as an unpacked integer
--
-- @since 0.1.0
sizeOfPVar# :: forall s a. Prim a => PVar s a -> Int#
sizeOfPVar# _ = sizeOf# (undefined :: a)
{-# INLINE sizeOfPVar# #-}

-- | Get the alignment of the mutable variable in bytes as an unpacked integer
--
-- @since 0.1.0
alignmentPVar# :: forall s a. Prim a => PVar s a -> Int#
alignmentPVar# _ = alignment# (undefined :: a)
{-# INLINE alignmentPVar# #-}


-- | Size in bytes of a value stored inside the mutable variable. `PVar` itself is neither
-- accessed nor evaluated.
--
-- @since 0.1.0
sizeOfPVar :: Prim a => PVar s a -> Int
sizeOfPVar pvar = I# (sizeOfPVar# pvar)
{-# INLINE sizeOfPVar #-}

-- | Alignment in bytes of the value stored inside of the mutable variable. `PVar` itself is
-- neither accessed nor evaluated.
--
-- @since 0.1.0
alignmentPVar :: Prim a => PVar s a -> Int
alignmentPVar pvar = I# (alignmentPVar# pvar)
{-# INLINE alignmentPVar #-}


unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}



-- | Check if `PVar` is backed by pinned memory or not
--
-- @since 0.1.0
isPinnedPVar :: PVar s a -> Bool
isPinnedPVar (PVar mba#) = isTrue# (isMutableByteArrayPinned# mba#)
{-# INLINE isPinnedPVar #-}


-- | Using `casIntArray#` perform atomic modification of an integer element in a
-- `MutableByteArray#`. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyIntArray# ::
     MutableByteArray# d -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (Int# -> (# Int#, b #)) -- ^ Function to be applied atomically to the element
  -> State# d -- ^ Starting state
  -> (# State# d, b #)
atomicModifyIntArray# mba# i# f s0# =
  let go s# o# =
        case f o# of
          (# n#, artifact #) ->
            case casIntArray# mba# i# o# n# s# of
              (# s'#, o'# #) ->
                case o# ==# o'# of
                  0# -> go s# o'#
                  _ -> seq# artifact s'#
   in case atomicReadIntArray# mba# i# s0# of
        (# s'#, o# #) -> go s'# o#
{-# INLINE atomicModifyIntArray# #-}



-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier.
--
-- @since 0.1.0
atomicModifyIntPVar ::
     PrimMonad m => PVar (PrimState m) Int -> (Int -> (Int, a)) -> m a
atomicModifyIntPVar (PVar mba#) f = primitive (atomicModifyIntArray# mba# 0# g)
  where
    g i# =
      case f (I# i#) of
        (I# o#, a) -> (# o#, a #)
    {-# INLINE g #-}
{-# INLINE atomicModifyIntPVar #-}


-- | Uses `casIntArray#` to perform atomic modification of an integer element in a
-- `MutableByteArray#`. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyIntArray_# ::
     MutableByteArray# d -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (Int# -> Int#) -- ^ Function to be applied atomically to the element
  -> State# d -- ^ Starting state
  -> State# d
atomicModifyIntArray_# mba# i# f s0# =
  let go s# o# =
        case casIntArray# mba# i# o# (f o#) s# of
          (# s'#, o'# #) ->
            case o# ==# o'# of
              0# -> go s# o'#
              _ -> s'#
   in case atomicReadIntArray# mba# i# s0# of
        (# s'#, o# #) -> go s'# o#
{-# INLINE atomicModifyIntArray_# #-}


-- | Apply a function to an integer element of a `PVar` atomically. Returns the old
-- value. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyIntPVar_ ::
     PrimMonad m => PVar (PrimState m) Int -> (Int -> Int) -> m ()
atomicModifyIntPVar_ (PVar mba#) f =
  primitive_ (atomicModifyIntArray_# mba# 0# (\i# -> unI# (f (I# i#))))
{-# INLINE atomicModifyIntPVar_ #-}
