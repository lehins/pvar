{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Primitive.PVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar
  ( -- | It has significantly better performance
    -- characterisitcs over `Data.IORef.IORef`, `Data.STRef.STRef` and
    -- `Data.Primtive.MutVar.MutVar`. This is because value is mutated directly in memory
    -- instead of following an extra pointer. Besides performance another consequence of this
    -- is that values are always evaluated to normal form when being written into the `Pvar`

  -- * Primitive variable
    PVar
  , newPVar
  , withPVarST
  -- * Generic Operations
  , readPVar
  , writePVar
  , modifyPVar
  , modifyPVar_
  , modifyPVarM
  , modifyPVarM_
  , swapPVars
  , swapPVars_
  , copyPVar
  , sizeOfPVar
  , alignmentPVar
  -- * Pinned memory
  , newPinnedPVar
  , newAlignedPinnedPVar
  , withPtrPVar
  , copyPVarToPtr
  , toForeignPtrPVar
  , isPinnedPVar
  -- -- * Numeric infix operations
  -- , (=+)
  -- , (=-)
  -- , (=*)
  -- , (=/)
  -- , (=%)
  -- ** Atomic operations
  , atomicModifyIntPVar
  , atomicModifyIntPVar_
  , atomicReadIntPVar
  , atomicWriteIntPVar
  , casIntPVar
  , atomicAddIntPVar
  , atomicSubIntPVar
  , atomicAndIntPVar
  , atomicNandIntPVar
  , atomicOrIntPVar
  , atomicXorIntPVar
  -- ** Re-exports
  , Prim
  , PrimMonad(PrimState)
  , RealWorld
  , ST
  , runST
  ) where

import Control.Monad (void)
import Control.Monad.ST (ST, runST)
import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_, touch)
import Data.Primitive.PVar.Internal
import Data.Primitive.PVar.Unsafe
import Data.Primitive.Types
import GHC.Exts
import GHC.ForeignPtr


-- | Check if `PVar` is backed by pinned memory or not
isPinnedPVar :: PVar s a -> Bool
isPinnedPVar (PVar mba#) = isTrue# (isMutableByteArrayPinned# mba#)
{-# INLINE isPinnedPVar #-}

-- | Run an ST action on a mutable PVar variable.
withPVarST ::
     Prim p
  => p -- ^ Initial value assigned to the mutable variable
  -> (forall s. PVar s p -> ST s a) -- ^ Action to run
  -> a -- ^ Reslt produced by the ST action
withPVarST x st = runST (newPVar x >>= st)
{-# INLINE withPVarST #-}

-- | Apply an action to the Ptr that references the mutable variable, but only if it is
-- backed by pinned memory, cause otherwise it would not be safe.
withPtrPVar :: (PrimMonad m, Prim a) => PVar s a -> (Ptr a -> m b) -> m (Maybe b)
withPtrPVar pvar f =
  case toPtrPVar pvar of
    Nothing -> return Nothing
    Just ptr -> do
      r <- f ptr
      touch pvar
      return $ Just r
{-# INLINE withPtrPVar #-}

-- | Convert `PVar` into a `ForeignPtr`, but only if it is backed by pinned memory.
toForeignPtrPVar :: PVar RealWorld a -> Maybe (ForeignPtr a)
toForeignPtrPVar pvar@(PVar mba#) =
  fmap (\(Ptr addr#) -> ForeignPtr addr# (PlainPtr mba#)) (toPtrPVar pvar)
{-# INLINE toForeignPtrPVar #-}

-- | Copy contents of one mutable variable `PVar` into another
copyPVar ::
     (PrimMonad m, Prim a)
  => PVar (PrimState m) a -- ^ Source variable
  -> PVar (PrimState m) a -- ^ Destination variable
  -> m ()
copyPVar pvar@(PVar mbas#) (PVar mbad#) =
  primitive_ (copyMutableByteArray# mbas# 0# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyPVar #-}

-- | Copy contents of a mutable variable `PVar` into a pointer `Ptr`
copyPVarToPtr :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> Ptr a -> m ()
copyPVarToPtr pvar@(PVar mbas#) (Ptr addr#) =
  primitive_ (copyMutableByteArrayToAddr# mbas# 0# addr# (sizeOfPVar# pvar))
{-# INLINE copyPVarToPtr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
modifyPVar :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> (a -> a) -> m a
modifyPVar pvar f = do
  a <- readPVar pvar
  writePVar pvar (f a)
  return a
{-# INLINE modifyPVar #-}

-- | Apply a pure function to the contents of a mutable variable.
modifyPVar_ :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> (a -> a) -> m ()
modifyPVar_ pvar f = readPVar pvar >>= writePVar pvar . f
{-# INLINE modifyPVar_ #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
modifyPVarM :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> (a -> m a) -> m a
modifyPVarM pvar f = do
  a <- readPVar pvar
  a' <- f a
  writePVar pvar a'
  return a
{-# INLINE modifyPVarM #-}

-- | Apply a monadic action to the contents of a mutable variable.
modifyPVarM_ :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> (a -> m a) -> m ()
modifyPVarM_ pvar f = readPVar pvar >>= f >>= writePVar pvar
{-# INLINE modifyPVarM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
swapPVars :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> PVar (PrimState m) a -> m (a, a)
swapPVars pvar1 pvar2 = do
  a1 <- readPVar pvar1
  a2 <- modifyPVar pvar2 (const a1)
  writePVar pvar1 a2
  return (a1, a2)
{-# INLINE swapPVars #-}

-- | Swap contents of two mutable variables.
swapPVars_ :: (PrimMonad m, Prim a) => PVar (PrimState m) a -> PVar (PrimState m) a -> m ()
swapPVars_ pvar1 pvar2 = void $ swapPVars pvar1 pvar2
{-# INLINE swapPVars_ #-}


(=+) :: (PrimMonad m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
(=+) pvar a = modifyPVar_ pvar (+ a)
{-# INLINE (=+) #-}

(=-) :: (PrimMonad m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
(=-) pvar a = modifyPVar_ pvar (subtract a)
{-# INLINE (=-) #-}

(=*) :: (PrimMonad m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
(=*) pvar a = modifyPVar_ pvar (* a)
{-# INLINE (=*) #-}

(=/) :: (PrimMonad m, Prim a, Fractional a) => PVar (PrimState m) a -> a -> m ()
(=/) pvar a = modifyPVar_ pvar (/ a)
{-# INLINE (=/) #-}

-- | C like modulo operator
(=%) :: (PrimMonad m, Prim a, Integral a) => PVar (PrimState m) a -> a -> m ()
(=%) pvar a = modifyPVar_ pvar (`mod` a)
{-# INLINE (=%) #-}


-- | Create a new `PVar` in pinned memory with an initial value in it aligned on the size of
-- an `Int`. Implies a full memory barrier.
atomicReadIntPVar :: PrimMonad m => PVar (PrimState m) Int -> m Int
atomicReadIntPVar (PVar mba#) =
  primitive $ \s# ->
    case atomicReadIntArray# mba# 0# s# of
      (# s'#, i# #) -> (# s'#, I# i# #)
{-# INLINE atomicReadIntPVar #-}

-- | Write a value into an `PVar` atomically. Implies a full memory barrier.
atomicWriteIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m ()
atomicWriteIntPVar (PVar mba#) a = primitive_ (atomicWriteIntArray# mba# 0# (unI# a))
{-# INLINE atomicWriteIntPVar #-}


-- | Compare and swap. This is a function that is used to implement `modifyIntPVar`. Implies a
-- full memory barrier.
casIntPVar ::
     PrimMonad m
  => PVar (PrimState m) Int -- ^ Variable to mutate
  -> Int -- ^ Old expected value
  -> Int -- ^ New value
  -> m Int -- ^ Old actual value
casIntPVar (PVar mba#) old new =
  primitive $ \s# ->
    case casIntArray# mba# 0# (unI# old) (unI# new) s# of
      (# s'#, i'# #) -> (# s'#, I# i'# #)
{-# INLINE casIntPVar #-}



-- | Add two numbers, corresponds to @`+`@ done atomically. Implies a full memory
-- barrier. Infix version: `!+`.
atomicAddIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m Int
atomicAddIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchAddIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicAddIntPVar #-}

-- | Subtract two numbers, corresponds to @`subtract`@ done atomically. Implies a full
-- memory barrier. Infix version: `!-`
atomicSubIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m Int
atomicSubIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchSubIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicSubIntPVar #-}


-- | Binary conjuction (AND), corresponds to @`and`@ done atomically. Implies a full memory
-- barrier. Infix version: `!&`
atomicAndIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m Int
atomicAndIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchAndIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicAndIntPVar #-}


-- | Binary negation of conjuction (Not AND), corresponds to @\\x y -> `complement` (x `and`
-- y)@ done atomically. Implies a full memory barrier. Infix version: `!~&`
atomicNandIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m Int
atomicNandIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchNandIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicNandIntPVar #-}


-- | Binary disjunction (OR), corresponds to `or` done atomically. Implies a full memory
-- barrier. Infix version: `!|`
atomicOrIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m Int
atomicOrIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchOrIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicOrIntPVar #-}


-- | Binary exclusive disjunction (XOR), corresponds to `xor` done atomically. Implies a
-- full memory barrier. Infix version: `!^`
atomicXorIntPVar :: PrimMonad m => PVar (PrimState m) Int -> Int -> m Int
atomicXorIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchXorIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicXorIntPVar #-}


-- | Binary negation (NOT), corresponds to ones' `complement` done atomically. Implies a full
-- memory barrier. Infix version: `!~`
atomicNotIntPVar :: PrimMonad m => PVar (PrimState m) Int -> m Int
atomicNotIntPVar (PVar mba#) =
  primitive $ \s# ->
    case fetchXorIntArray# mba# 0# fullInt# s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
  where
    fullInt# =
      case maxBound :: Word of
        W# w# -> word2Int# w#
{-# INLINE atomicNotIntPVar #-}

