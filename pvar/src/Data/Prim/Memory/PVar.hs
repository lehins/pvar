{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Data.Prim.Memory.PVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Prim.Memory.PVar
  ( -- | `PVar` has significantly better performance characteristics over
    -- `Data.IORef.IORef`, `Data.STRef.STRef` and `Data.Primitive.MutVar.MutVar`. This is
    -- because value is mutated directly in memory instead of following an extra
    -- pointer. Besides better performance there is another consequence of direct
    -- mutation, namely the value is always evaluated to normal form when being written
    -- into a `PVar`

    PVar
  -- * Creation
  , newPVar
  , withPVarST
  -- * Mutable Operations
  , readPVar
  , writePVar
  , modifyPVar
  , modifyPVar_
  , modifyFetchOldPVar
  , modifyFetchNewPVar
  , modifyPVarM
  , modifyPVarM_
  , modifyFetchOldPVarM
  , modifyFetchNewPVarM
  , swapPVars_
  , swapPVars
  , copyPVar
  , sizeOfPVar
  , alignmentPVar
  -- * Pinned memory
  , newPinnedPVar
  , newAlignedPinnedPVar
  , withPtrPVar
  , withNewPtrPVar
  , copyPVarToPtr
  , toForeignPtrPVar
  , isPinnedPVar
  -- -- * Numeric infix operations
  -- , (=+)
  -- , (=-)
  -- , (=*)
  -- , (=/)
  -- , (=%)
  -- * Atomic operations
  , atomicModifyPVar
  , atomicModifyPVar_
  , atomicModifyFetchOldPVar
  , atomicModifyFetchNewPVar
  , atomicReadPVar
  , atomicWritePVar
  , atomicAddFetchOldPVar
  , atomicAddFetchNewPVar
  , atomicSubFetchOldPVar
  , atomicSubFetchNewPVar
  , atomicAndFetchOldPVar
  , atomicAndFetchNewPVar
  , atomicNandFetchOldPVar
  , atomicNandFetchNewPVar
  , atomicOrFetchOldPVar
  , atomicOrFetchNewPVar
  , atomicXorFetchOldPVar
  , atomicXorFetchNewPVar
  , atomicNotFetchOldPVar
  , atomicNotFetchNewPVar
  , casPVar
  -- * Re-exports
  , module Data.Prim
  ) where

import Control.Monad.ST (ST, runST)
import Control.Prim.Monad
import Data.Prim
import Data.Prim.Atomic
import Data.Prim.Memory.PVar.Internal
import Data.Prim.Memory.PVar.Unsafe
import Foreign.Prim


-- | Run an `ST` action on a mutable variable.
--
-- @since 0.1.0
withPVarST ::
     Prim p
  => p -- ^ Initial value assigned to the mutable variable
  -> (forall s. PVar p s -> ST s a) -- ^ Action to run
  -> a -- ^ Result produced by the `ST` action
withPVarST x st = runST (newPVar x >>= st)
{-# INLINE withPVarST #-}

-- | Apply an action to the `Ptr` that references the mutable variable, but only if it is
-- backed by pinned memory, cause otherwise it would be unsafe.
--
-- @since 0.1.0
withPtrPVar :: (MonadPrim s m, Prim a) => PVar a n -> (Ptr a -> m b) -> m (Maybe b)
withPtrPVar pvar f =
  case toPtrPVar pvar of
    Nothing -> return Nothing
    Just ptr -> do
      r <- f ptr
      touch pvar
      return $ Just r
{-# INLINE withPtrPVar #-}

-- | Apply an action to the newly allocated mutable variable and the `Ptr` that references
-- it. It is safe to return the `PVar`, but not the `Ptr`
--
-- @since 2.0.0
withNewPtrPVar :: (MonadPrim s m, Prim e) => e -> (PVar e s -> Ptr e -> m b) -> m b
withNewPtrPVar e f = do
  pvar <- newAlignedPinnedPVar e
  unsafeWithPtrPVar pvar f
{-# INLINE withNewPtrPVar #-}

-- | Convert `PVar` into a `ForeignPtr`, but only if it is backed by pinned memory.
--
-- @since 0.1.0
toForeignPtrPVar :: PVar a s -> Maybe (ForeignPtr a)
toForeignPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToForeignPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toForeignPtrPVar #-}

-- | Copy contents of one mutable variable `PVar` into another
--
-- @since 0.1.0
copyPVar ::
     (MonadPrim s m, Prim a)
  => PVar a s -- ^ Source variable
  -> PVar a s -- ^ Destination variable
  -> m ()
copyPVar pvar@(PVar mbas#) (PVar mbad#) =
  prim_ (copyMutableByteArray# mbas# 0# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyPVar #-}

-- | Copy contents of a mutable variable `PVar` into a pointer `Ptr`
--
-- @since 0.1.0
copyPVarToPtr :: (MonadPrim s m, Prim a) => PVar a s -> Ptr a -> m ()
copyPVarToPtr pvar@(PVar mbas#) (Ptr addr#) =
  prim_ (copyMutableByteArrayToAddr# mbas# 0# addr# (sizeOfPVar# pvar))
{-# INLINE copyPVarToPtr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyPVar :: (MonadPrim s m, Prim a) => PVar a s -> (a -> (a, b)) -> m b
modifyPVar pvar f = modifyPVarM pvar (return . f)
{-# INLINE modifyPVar #-}

-- | Apply a pure function to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVar_ :: (MonadPrim s m, Prim a) => PVar a s -> (a -> a) -> m ()
modifyPVar_ pvar f = modifyPVarM_ pvar (return . f)
{-# INLINE modifyPVar_ #-}


-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
--
-- @since 2.0.0
modifyFetchOldPVar :: (MonadPrim s m, Prim a) => PVar a s -> (a -> a) -> m a
modifyFetchOldPVar pvar f = modifyFetchOldPVarM pvar (return . f)
{-# INLINE modifyFetchOldPVar #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the new value.
--
-- @since 2.0.0
modifyFetchNewPVar :: (MonadPrim s m, Prim a) => PVar a s -> (a -> a) -> m a
modifyFetchNewPVar pvar f = modifyFetchNewPVarM pvar (return . f)
{-# INLINE modifyFetchNewPVar #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyPVarM :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m (a, b)) -> m b
modifyPVarM pvar f = do
  a <- readPVar pvar
  (a', b) <- f a
  b <$ writePVar pvar a'
{-# INLINE modifyPVarM #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
--
-- @since 2.0.0
modifyFetchOldPVarM :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m a) -> m a
modifyFetchOldPVarM pvar f = do
  a <- readPVar pvar
  a <$ (writePVar pvar =<< f a)
{-# INLINE modifyFetchOldPVarM #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the new value.
--
-- @since 2.0.0
modifyFetchNewPVarM :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m a) -> m a
modifyFetchNewPVarM pvar f = do
  a <- readPVar pvar
  a' <- f a
  a' <$ writePVar pvar a'
{-# INLINE modifyFetchNewPVarM #-}


-- | Apply a monadic action to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVarM_ :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m a) -> m ()
modifyPVarM_ pvar f = readPVar pvar >>= f >>= writePVar pvar
{-# INLINE modifyPVarM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 0.1.0
swapPVars :: (MonadPrim s m, Prim a) => PVar a s -> PVar a s -> m (a, a)
swapPVars pvar1 pvar2 = do
  a1 <- readPVar pvar1
  a2 <- modifyFetchOldPVar pvar2 (const a1)
  (a1, a2) <$ writePVar pvar1 a2
{-# INLINE swapPVars #-}

-- | Swap contents of two mutable variables.
--
-- @since 0.1.0
swapPVars_ :: (MonadPrim s m, Prim a) => PVar a s -> PVar a s -> m ()
swapPVars_ pvar1 pvar2 = void $ swapPVars pvar1 pvar2
{-# INLINE swapPVars_ #-}

-- TODO: Come up with a concrete interface for numerics
-- (=+) :: (MonadPrim s m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=+) pvar a = modifyPVar_ pvar (+ a)
-- {-# INLINE (=+) #-}

-- (=-) :: (MonadPrim s m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=-) pvar a = modifyPVar_ pvar (subtract a)
-- {-# INLINE (=-) #-}

-- (=*) :: (MonadPrim s m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=*) pvar a = modifyPVar_ pvar (* a)
-- {-# INLINE (=*) #-}

-- (=/) :: (MonadPrim s m, Prim a, Fractional a) => PVar (PrimState m) a -> a -> m ()
-- (=/) pvar a = modifyPVar_ pvar (/ a)
-- {-# INLINE (=/) #-}

-- -- | C like modulo operator
-- (=%) :: (MonadPrim s m, Prim a, Integral a) => PVar (PrimState m) a -> a -> m ()
-- (=%) pvar a = modifyPVar_ pvar (`mod` a)
-- {-# INLINE (=%) #-}


-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier. Returns some arbitrary artifact of computation.
--
-- @since 0.1.0
atomicModifyPVar ::
     (Atomic e, MonadPrim s m) => PVar e s -> (e -> (e, a)) -> m a
atomicModifyPVar (PVar mba#) f = prim (atomicModifyMutableByteArray# mba# 0# g)
  where
    g e =
      case f e of
        (e', a) -> (# e', a #)
    {-# INLINE g #-}
{-# INLINE atomicModifyPVar #-}


-- | Apply a function to the contents of a `PVar` atomically. Implies a full memory
-- barrier.
--
-- @since 0.1.0
atomicModifyPVar_ :: (Atomic e, MonadPrim s m) => PVar e s -> (e -> e) -> m ()
atomicModifyPVar_ (PVar mba#) f =
  prim_ (atomicModifyMutableByteArray_# mba# 0# f)
{-# INLINE atomicModifyPVar_ #-}




-- | Read a value from `PVar` atomically. Implies a full memory barrier.
--
-- @since 0.1.0
atomicReadPVar :: (Atomic e, MonadPrim s m) => PVar e s -> m e
atomicReadPVar (PVar mba#) = prim $ atomicReadMutableByteArray# mba# 0#
{-# INLINE atomicReadPVar #-}

-- | Write a value into an `PVar` atomically. Implies a full memory barrier.
--
-- @since 0.1.0
atomicWritePVar :: (Atomic e, MonadPrim s m) => PVar e s -> e -> m ()
atomicWritePVar (PVar mba#) a = prim_ (atomicWriteMutableByteArray# mba# 0# a)
{-# INLINE atomicWritePVar #-}


-- | Apply a function to the value of a `PVar` atomically. Implies a full memory
-- barrier. Returns the new value.
--
-- @since 2.0.0
atomicModifyFetchNewPVar ::
     (Atomic e, MonadPrim s m) => PVar e s -> (e -> e) -> m e
atomicModifyFetchNewPVar (PVar mba#) f =
  prim $ atomicModifyFetchNewMutableByteArray# mba# 0# f
{-# INLINE atomicModifyFetchNewPVar #-}

-- | Apply a function to the value of a `PVar` atomically. Implies a full memory
-- barrier. Returns the previous value.
--
-- @since 2.0.0
atomicModifyFetchOldPVar ::
     (Atomic e, MonadPrim s m) => PVar e s -> (e -> e) -> m e
atomicModifyFetchOldPVar (PVar mba#) f =
  prim $ atomicModifyFetchOldMutableByteArray# mba# 0# f
{-# INLINE atomicModifyFetchOldPVar #-}


-- | Compare and swap. Implies a full memory barrier.
--
-- @since 2.0.0
casPVar ::
     (Atomic e, MonadPrim s m)
  => PVar e s -- ^ Variable to mutate
  -> e -- ^ Old expected value
  -> e -- ^ New value
  -> m e -- ^ Old actual value
casPVar (PVar mba#) old new =
  prim $ casMutableByteArray# mba# 0# old new
{-# INLINE casPVar #-}



-- | Add two numbers, corresponds to @(`+`)@ done atomically. Returns the previous value of
-- the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicAddFetchOldPVar :: (AtomicCount e, MonadPrim s m) => PVar e s -> e -> m e
atomicAddFetchOldPVar (PVar mba#) a =
  prim $ atomicAddFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicAddFetchOldPVar #-}

-- | Add two numbers, corresponds to @(`+`)@ done atomically. Returns the new value of
-- the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicAddFetchNewPVar :: (AtomicCount e, MonadPrim s m) => PVar e s -> e -> m e
atomicAddFetchNewPVar (PVar mba#) a =
  prim $ atomicAddFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicAddFetchNewPVar #-}

-- | Subtract two numbers, corresponds to @(`-`)@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicSubFetchOldPVar :: (AtomicCount e, MonadPrim s m) => PVar e s -> e -> m e
atomicSubFetchOldPVar (PVar mba#) a =
  prim $ atomicSubFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicSubFetchOldPVar #-}

-- | Subtract two numbers, corresponds to @(`-`)@ done atomically. Returns the
-- new value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicSubFetchNewPVar :: (AtomicCount e, MonadPrim s m) => PVar e s -> e -> m e
atomicSubFetchNewPVar (PVar mba#) a =
  prim $ atomicSubFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicSubFetchNewPVar #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Returns
-- the previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicAndFetchOldPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicAndFetchOldPVar (PVar mba#) a =
  prim $ atomicAndFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicAndFetchOldPVar #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Returns
-- the new value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicAndFetchNewPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicAndFetchNewPVar (PVar mba#) a =
  prim $ atomicAndFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicAndFetchNewPVar #-}


-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x
-- `Data.Bits..&.` y)@ done atomically. Returns the previous value of the mutable variable. Implies
-- a full memory barrier.
--
-- @since 2.0.0
atomicNandFetchOldPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicNandFetchOldPVar (PVar mba#) a =
  prim $ atomicNandFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicNandFetchOldPVar #-}

-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x
-- `Data.Bits..&.` y)@ done atomically. Returns the new value of the mutable variable. Implies
-- a full memory barrier.
--
-- @since 2.0.0
atomicNandFetchNewPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicNandFetchNewPVar (PVar mba#) a =
  prim $ atomicNandFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicNandFetchNewPVar #-}



-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicOrFetchOldPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicOrFetchOldPVar (PVar mba#) a =
  prim $ atomicOrFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicOrFetchOldPVar #-}


-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the new
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicOrFetchNewPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicOrFetchNewPVar (PVar mba#) a =
  prim $ atomicOrFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicOrFetchNewPVar #-}


-- | Binary exclusive disjunction (XOR), corresponds to @`Data.Bits.xor`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicXorFetchOldPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicXorFetchOldPVar (PVar mba#) a =
  prim $ atomicXorFetchOldMutableByteArray# mba# 0# a
{-# INLINE atomicXorFetchOldPVar #-}

-- | Binary exclusive disjunction (XOR), corresponds to @`Data.Bits.xor`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicXorFetchNewPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> e -> m e
atomicXorFetchNewPVar (PVar mba#) a =
  prim $ atomicXorFetchNewMutableByteArray# mba# 0# a
{-# INLINE atomicXorFetchNewPVar #-}


-- | Binary negation (NOT), corresponds to ones' @`Data.Bits.complement`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicNotFetchOldPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> m e
atomicNotFetchOldPVar (PVar mba#) =
  prim $ atomicNotFetchOldMutableByteArray# mba# 0#
{-# INLINE atomicNotFetchOldPVar #-}


-- | Binary negation (NOT), corresponds to ones' @`Data.Bits.complement`@ done atomically. Returns the
-- new value of the mutable variable. Implies a full memory barrier.
--
-- @since 2.0.0
atomicNotFetchNewPVar :: (AtomicBits e, MonadPrim s m) => PVar e s -> m e
atomicNotFetchNewPVar (PVar mba#) =
  prim $ atomicNotFetchNewMutableByteArray# mba# 0#
{-# INLINE atomicNotFetchNewPVar #-}
