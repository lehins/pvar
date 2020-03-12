{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
  , sizeOfPVar#
  -- * Atomic operations
  , unI#
  , atomicModifyIntArray#
  )
  where

import GHC.Exts
import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_, touch)
import Data.Primitive.Types
import qualified Foreign.Storable as S

-- | Mutable variable with primitive value. It has significantly better performance
-- characterisitcs over an `Data.IORef.IORef` or an `Data.STRef.STRef`
--
-- @since 0.1.0
data PVar s a = PVar (MutableByteArray# s)

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

newPVar :: (PrimMonad m, Prim a) => a -> m (PVar (PrimState m) a)
newPVar v =
  primitive $ \s# ->
    case newByteArray# 1# s# of
      (# s'#, mba# #) -> (# writeByteArray# mba# 0# v s'#, PVar mba# #)
{-# INLINE newPVar #-}

newPinnedPVar :: (PrimMonad m, Prim a) => a -> m (PVar (PrimState m) a)
newPinnedPVar v =
  primitive $ \s# ->
    case newPinnedByteArray# 1# s# of
      (# s'#, mba# #) -> (# writeByteArray# mba# 0# v s'#, PVar mba# #)
{-# INLINE newPinnedPVar #-}

newAlignedPinnedPVar :: (PrimMonad m, Prim a) => a -> m (PVar (PrimState m) a)
newAlignedPinnedPVar v =
  primitive $ \s# ->
    case newAlignedPinnedByteArray# 1# (alignment# v) s# of
      (# s'#, mba# #) -> (# writeByteArray# mba# 0# v s'#, PVar mba# #)
{-# INLINE newAlignedPinnedPVar #-}


readPVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> m a
readPVar (PVar mba#) = primitive (readByteArray# mba# 0#)
{-# INLINE readPVar #-}

writePVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> a -> m ()
writePVar (PVar mba#) v = primitive_ (writeByteArray# mba# 0# v)
{-# INLINE writePVar #-}

sizeOfPVar# :: forall s a. Prim a => PVar s a -> Int#
sizeOfPVar# _ = sizeOf# (undefined :: a)
{-# INLINE sizeOfPVar# #-}


unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}



-- | Using `casIntArray#` perform atomic modification of an integer element in a
-- `MutableByteArray#`. Implies a full memory barrier.
atomicModifyIntArray# ::
     MutableByteArray# d -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (Int# -> Int#) -- ^ Function to be applied atomically to the element
  -> State# d -- ^ Starting state
  -> (# State# d, Int# #)
atomicModifyIntArray# mba# i# f s0# =
  let go s# o# =
        case casIntArray# mba# i# o# (f o#) s# of
          (# s'#, o'# #) ->
            case o# ==# o'# of
              0# -> go s# o'#
              _ -> (# s'#, o# #)
   in case atomicReadIntArray# mba# i# s0# of
        (# s'#, o# #) -> go s'# o#
{-# INLINE atomicModifyIntArray# #-}
