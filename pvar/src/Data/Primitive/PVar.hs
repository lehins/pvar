{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Data.Primitive.PVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar
  ( -- | `PVar` has significantly better performance characterisitcs over
    -- `Data.IORef.IORef`, `Data.STRef.STRef` and `Data.Primtive.MutVar.MutVar`. This is
    -- because value is mutated directly in memory instead of following an extra
    -- pointer. Besides better performance there is another consequence of direct
    -- mutation, namely that values are always evaluated to normal form when being written
    -- into a `PVar`

  -- * Primitive variable
    PVar
  , newPVar
  , withPVarST
  -- * Generic Operations
  , readPVar
  , writePVar
  , modifyPVar_
  , modifyPVar
  , modifyPVarM_
  , modifyPVarM
  , swapPVars_
  , swapPVars
  , copyPVar
  , sizeOfPVar
  , alignmentPVar
  -- * Pinned memory
  --
  -- $pinned
  , newPinnedPVar
  , newAlignedPinnedPVar
  , withPtrPVar
  , withStorablePVar
  , withAlignedStorablePVar
  , copyPVarToPtr
  , toForeignPtrPVar
  , isPinnedPVar
  , peekPrim, pokePrim
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
  , atomicNotIntPVar
  -- ** Re-exports
  , Prim
  , PrimMonad(PrimState)
  , RealWorld
  , sizeOf
  , alignment
  , ST
  , runST
  , S.Storable(peek, poke)
  ) where

import Control.Monad (void)
import Control.Monad.Primitive (PrimMonad(primitive), PrimState, primitive_,
                                touch)
import Control.Monad.ST (ST, runST)
import Data.Primitive.PVar.Internal
import Data.Primitive.PVar.Unsafe
import Data.Primitive (sizeOf, alignment)
import Data.Primitive.Types
import qualified Foreign.Storable as S
import GHC.Exts
import GHC.ForeignPtr

-- $pinned
-- In theory it is unsafe to mix `S.Storable` and `Prim` operations on the same chunk of
-- memory, because some instances can have differnet memory layouts for the same
-- type. This is highly uncommon in practice and if you are intermixing the two concepts
-- together you probably already know what you are doing.



-- | Run an `ST` action on a mutable variable.
--
-- @since 0.1.0
withPVarST ::
     Prim p
  => p -- ^ Initial value assigned to the mutable variable
  -> (forall s. PVar (ST s) p -> ST s a) -- ^ Action to run
  -> a -- ^ Result produced by the `ST` action
withPVarST x st = runST (newPVar x >>= st)
{-# INLINE withPVarST #-}

-- | Apply an action to the `Ptr` that references the mutable variable, but only if it is
-- backed by pinned memory, cause otherwise it would be unsafe.
--
-- @since 0.1.0
withPtrPVar :: (PrimMonad m, Prim a) => PVar n a -> (Ptr a -> m b) -> m (Maybe b)
withPtrPVar pvar f =
  case toPtrPVar pvar of
    Nothing -> return Nothing
    Just ptr -> do
      r <- f ptr
      touch pvar
      return $ Just r
{-# INLINE withPtrPVar #-}

-- | Convert `PVar` into a `ForeignPtr`, but only if it is backed by pinned memory.
--
-- @since 0.1.0
toForeignPtrPVar :: PVar IO a -> Maybe (ForeignPtr a)
toForeignPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToForeignPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toForeignPtrPVar #-}

-- | Copy contents of one mutable variable `PVar` into another
--
-- @since 0.1.0
copyPVar ::
     (PrimMonad m, Prim a)
  => PVar m a -- ^ Source variable
  -> PVar m a -- ^ Destination variable
  -> m ()
copyPVar pvar@(PVar mbas#) (PVar mbad#) =
  primitive_ (copyMutableByteArray# mbas# 0# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyPVar #-}

-- | Copy contents of a mutable variable `PVar` into a pointer `Ptr`
--
-- @since 0.1.0
copyPVarToPtr :: (PrimMonad m, Prim a) => PVar m a -> Ptr a -> m ()
copyPVarToPtr pvar@(PVar mbas#) (Ptr addr#) =
  primitive_ (copyMutableByteArrayToAddr# mbas# 0# addr# (sizeOfPVar# pvar))
{-# INLINE copyPVarToPtr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
--
-- @since 0.1.0
modifyPVar :: (PrimMonad m, Prim a) => PVar m a -> (a -> a) -> m a
modifyPVar pvar f = modifyPVarM pvar (return . f)
{-# INLINE modifyPVar #-}

-- | Apply a pure function to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVar_ :: (PrimMonad m, Prim a) => PVar m a -> (a -> a) -> m ()
modifyPVar_ pvar f = modifyPVarM_ pvar (return . f)
{-# INLINE modifyPVar_ #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
--
-- @since 0.1.0
modifyPVarM :: (PrimMonad m, Prim a) => PVar m a -> (a -> m a) -> m a
modifyPVarM pvar f = do
  a <- readPVar pvar
  a' <- f a
  writePVar pvar a'
  return a
{-# INLINE modifyPVarM #-}

-- | Apply a monadic action to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVarM_ :: (PrimMonad m, Prim a) => PVar m a -> (a -> m a) -> m ()
modifyPVarM_ pvar f = readPVar pvar >>= f >>= writePVar pvar
{-# INLINE modifyPVarM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 0.1.0
swapPVars :: (PrimMonad m, Prim a) => PVar m a -> PVar m a -> m (a, a)
swapPVars pvar1 pvar2 = do
  a1 <- readPVar pvar1
  a2 <- modifyPVar pvar2 (const a1)
  writePVar pvar1 a2
  return (a1, a2)
{-# INLINE swapPVars #-}

-- | Swap contents of two mutable variables.
--
-- @since 0.1.0
swapPVars_ :: (PrimMonad m, Prim a) => PVar m a -> PVar m a -> m ()
swapPVars_ pvar1 pvar2 = void $ swapPVars pvar1 pvar2
{-# INLINE swapPVars_ #-}

-- TODO: Come up with a concrete interface for numerics
-- (=+) :: (PrimMonad m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=+) pvar a = modifyPVar_ pvar (+ a)
-- {-# INLINE (=+) #-}

-- (=-) :: (PrimMonad m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=-) pvar a = modifyPVar_ pvar (subtract a)
-- {-# INLINE (=-) #-}

-- (=*) :: (PrimMonad m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=*) pvar a = modifyPVar_ pvar (* a)
-- {-# INLINE (=*) #-}

-- (=/) :: (PrimMonad m, Prim a, Fractional a) => PVar (PrimState m) a -> a -> m ()
-- (=/) pvar a = modifyPVar_ pvar (/ a)
-- {-# INLINE (=/) #-}

-- -- | C like modulo operator
-- (=%) :: (PrimMonad m, Prim a, Integral a) => PVar (PrimState m) a -> a -> m ()
-- (=%) pvar a = modifyPVar_ pvar (`mod` a)
-- {-# INLINE (=%) #-}




-- | Apply an action to the newly allocated `PVar` and to the `Ptr` that references
-- it. Memory allocated with number of bytes specified by @`S.sizeOf` a@ is allocated and
-- pinned, therefore it is safe to operate directly with the pointer as well as over
-- FFI. Returning the pointer from the supplied action would be very unsafe, therefore
-- return the `PVar` if you still need it afterwards, garbage collector will cleanup the
-- memory when it is no longer needed.
--
-- @since 0.1.0
withStorablePVar ::
     (PrimMonad m, S.Storable a)
  => a -- ^ Initial value
  -> (PVar m a -> Ptr a -> m b) -- ^ Action to run
  -> m b
withStorablePVar a f = do
  pvar <- rawStorablePVar
  runWithPokedPtr pvar a f
{-# INLINE withStorablePVar #-}

-- | Same `withStorablePVar`, except memory is aligned according to `S.alignment`.
--
-- @since 0.1.0
withAlignedStorablePVar ::
     (PrimMonad m, S.Storable a)
  => a -- ^ Initial value
  -> (PVar m a -> Ptr a -> m b) -- ^ Action to run
  -> m b
withAlignedStorablePVar a f = do
  pvar <- rawAlignedStorablePVar
  runWithPokedPtr pvar a f
{-# INLINE withAlignedStorablePVar #-}


-- | Create a new `PVar` in pinned memory with an initial value in it aligned on the size of
-- an `Int`. Implies a full memory barrier.
--
-- @since 0.1.0
atomicReadIntPVar :: PrimMonad m => PVar m Int -> m Int
atomicReadIntPVar (PVar mba#) =
  primitive $ \s# ->
    case atomicReadIntArray# mba# 0# s# of
      (# s'#, i# #) -> (# s'#, I# i# #)
{-# INLINE atomicReadIntPVar #-}

-- | Write a value into an `PVar` atomically. Implies a full memory barrier.
--
-- @since 0.1.0
atomicWriteIntPVar :: PrimMonad m => PVar m Int -> Int -> m ()
atomicWriteIntPVar (PVar mba#) a = primitive_ (atomicWriteIntArray# mba# 0# (unI# a))
{-# INLINE atomicWriteIntPVar #-}


-- | Compare and swap. This is also a function that is used to implement
-- `atomicModifyIntPVar`. Implies a full memory barrier.
--
-- @since 0.1.0
casIntPVar ::
     PrimMonad m
  => PVar m Int -- ^ Variable to mutate
  -> Int -- ^ Old expected value
  -> Int -- ^ New value
  -> m Int -- ^ Old actual value
casIntPVar (PVar mba#) old new =
  primitive $ \s# ->
    case casIntArray# mba# 0# (unI# old) (unI# new) s# of
      (# s'#, i'# #) -> (# s'#, I# i'# #)
{-# INLINE casIntPVar #-}



-- | Add two numbers, corresponds to @(`+`)@ done atomically. Returns the previous value of
-- the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicAddIntPVar :: PrimMonad m => PVar m Int -> Int -> m Int
atomicAddIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchAddIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicAddIntPVar #-}

-- | Subtract two numbers, corresponds to @(`-`)@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicSubIntPVar :: PrimMonad m => PVar m Int -> Int -> m Int
atomicSubIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchSubIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicSubIntPVar #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicAndIntPVar :: PrimMonad m => PVar m Int -> Int -> m Int
atomicAndIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchAndIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicAndIntPVar #-}


-- | Binary negation of conjuction (Not AND), corresponds to @\\x y -> `Data.Bits.complement` (x
-- `Data.Bits..&.` y)@ done atomically. Returns the previous value of the mutable variable. Implies
-- a full memory barrier.
--
-- @since 0.1.0
atomicNandIntPVar :: PrimMonad m => PVar m Int -> Int -> m Int
atomicNandIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchNandIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicNandIntPVar #-}


-- | Binary disjunction (OR), corresponds to `(`Data.Bits..|.)` done atomically. Returns the previous
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicOrIntPVar :: PrimMonad m => PVar m Int -> Int -> m Int
atomicOrIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchOrIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicOrIntPVar #-}


-- | Binary exclusive disjunction (XOR), corresponds to `Data.Bits.xor` done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicXorIntPVar :: PrimMonad m => PVar m Int -> Int -> m Int
atomicXorIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchXorIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicXorIntPVar #-}


-- | Binary negation (NOT), corresponds to ones' `Data.Bits.complement` done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicNotIntPVar :: PrimMonad m => PVar m Int -> m Int
atomicNotIntPVar (PVar mba#) =
  primitive $ \s# ->
    case fetchXorIntArray# mba# 0# fullInt# s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
  where
    fullInt# =
      case maxBound :: Word of
        W# w# -> word2Int# w#
{-# INLINE atomicNotIntPVar #-}

