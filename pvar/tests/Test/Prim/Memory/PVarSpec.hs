{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.Memory.PVarSpec (spec) where

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Monad
import Control.Prim.Monad
import Control.Monad.ST
import Data.Bits
import Data.Foldable as F
import Data.GenValidity
import Data.Int
import Data.Prim
import Data.List (partition)
import Data.Prim.Memory.PVar
import Data.Prim.Memory.PVar.Unsafe as Unsafe
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Prim.Ptr
import qualified Foreign.Storable as Storable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Function (apply)
import Test.QuickCheck.Monadic

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v

forAllST :: (Show p, Testable t) => Gen p -> (forall s. p -> ST s t) -> Property
forAllST g propM = forAll g $ \v -> monadicST $ run $ propM v


forAllPVarST ::
     (Show p, Prim p, Testable t)
  => Gen p
  -> (forall s. p -> PVar p s -> ST s t)
  -> Property
forAllPVarST g propM = forAllST g $ \v -> newPVar v >>= propM v

forAllPVarIO ::
     (Show p, Prim p, Testable t)
  => Gen p
  -> (p -> PVar p RealWorld -> IO t)
  -> Property
forAllPVarIO g propM = forAllIO g $ \v -> newPVar v >>= propM v

propPVarST ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (forall s. p -> PVar p s -> ST s t)
  -> Spec
propPVarST name gen action = prop name $ forAllPVarST gen action

propPVarIO ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (p -> PVar p RealWorld -> IO t)
  -> Spec
propPVarIO name gen action = prop name $ forAllPVarIO gen action

specPrim ::
     ( Show p
     , Eq p
     , Prim p
     , Storable.Storable p
     , Typeable p
     , Arbitrary p
     , CoArbitrary p
     , Function p
     )
  => p -- ^ Zero value
  -> Gen p
  -> (Gen p -> Spec)
  -> Spec
specPrim defZero gen extraSpec =
  describe ("PVar s " ++ showsType gen "") $ do
    propPVarIO "readPVar" gen $ \v pvar -- deepseq is used for coverage only
     -> pvar `deepseq` readPVar pvar `shouldReturn` v
    propPVarIO "writePVar/readPVar" gen $ \_ pvar ->
      return $
      forAll gen $ \v -> do
        writePVar pvar v
        readPVar pvar `shouldReturn` v
    prop "withPVarST" $
      forAll gen $ \a ->
        forAll gen $ \b ->
          withPVarST a $ \var -> do
            a' <- readPVar var
            writePVar var b
            b' <- readPVar var
            pure (a === a' .&&. b === b')
    propPVarIO "newPinnedPVar" gen $ \a var -> do
      pinnedVar <- newPinnedPVar a
      (===) <$> readPVar var <*> readPVar pinnedVar
    propPVarIO "newAlignedPinnedPVar" gen $ \a var -> do
      pinnedVar <- newAlignedPinnedPVar a
      (===) <$> readPVar var <*> readPVar pinnedVar
    propPVarIO "modifyPVar_" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar_ pvar (apply f)
        readPVar pvar `shouldReturn` apply f a
    propPVarIO "modifyPVar" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        let (a', b :: Int) = apply f a
        modifyPVar pvar (apply f) `shouldReturn` b
        readPVar pvar `shouldReturn` a'
    propPVarIO "modifyFetchOldPVar" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyFetchOldPVar pvar (apply f) `shouldReturn` a
        readPVar pvar `shouldReturn` apply f a
    propPVarIO "modifyFetchOldPVarM" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyFetchOldPVarM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ apply f a'
        a' `shouldBe` a
        readPVar pvar `shouldReturn` apply f a
    propPVarIO "modifyFetchNewPVar" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f ->
        modifyFetchNewPVar pvar (apply f) `shouldReturn` apply f a
    propPVarIO "modifyFetchNewPVarM" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyFetchNewPVarM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ apply f a'
        a' `shouldBe` apply f a
    propPVarIO "modifyPVarM_" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        modifyPVarM_ pvar $ \a' -> do
          a' `shouldBe` a
          pure $ apply f a'
        readPVar pvar `shouldReturn` apply f a
    propPVarIO "swapPVars" gen $ \a avar ->
      return $
      forAllPVarIO gen $ \b bvar -> do
        swapPVars avar bvar `shouldReturn` (a, b)
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propPVarIO "swapPVars_" gen $ \a avar ->
      return $
      forAllPVarIO gen $ \b bvar -> do
        swapPVars_ avar bvar
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propPVarIO "copyPVar" gen $ \a avar ->
      return $
      forAllPVarIO gen $ \_ bvar -> do
        copyPVar avar bvar
        readPVar bvar `shouldReturn` a
    propPVarST "sizeOfPVar" gen $ \a avar -> pure (sizeOfPVar avar === Storable.sizeOf a)
    propPVarST "alignmentPVar" gen $ \a avar ->
      pure (alignmentPVar avar === alignment a)
    describe "Unsafe" $ do
      propPVarST "sizeOf" gen $ \_ var -> pure (toPtrPVar var === Nothing)
    describe "Reset Memory" $
      propPVarIO "zeroPVar" gen $ \_ var -> do
        zeroPVar var
        readPVar var `shouldReturn` defZero
    extraSpec gen

specStorable ::
     (Show p, Eq p, Prim p, Storable.Storable p, Arbitrary p, CoArbitrary p, Function p)
  => Gen p
  -> Spec
specStorable gen =
  describe "Storable" $ do
    propPVarIO "withPVarPtr (newPVar)" gen $ \_ var ->
      withPtrPVar var pure `shouldReturn` Nothing
    prop "withPVarPtr (newPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newPinnedPVar a
        maybe (error "Expected to get a Just Ptr") pure =<<
          withPtrPVar var (\ptr -> readPtr ptr `shouldReturn` a)
    prop "withPVarPtr (newAlignedPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newAlignedPinnedPVar a
        maybe (error "Expected to get a Just Ptr") pure =<<
          withPtrPVar var (\ptr -> readPtr ptr `shouldReturn` a)
    propPVarIO "toForeignPtr (newPVar)" gen $ \_ var ->
      toForeignPtrPVar var `shouldBe` Nothing
    prop "toForeignPtr (newPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newPinnedPVar a
        fPtr <-
          maybe (error "Expected to get a Just ForeignPtr") pure $
          toForeignPtrPVar var
        withForeignPtr fPtr $ \ptr -> readPtr ptr `shouldReturn` a
    prop "toForeignPtr (newAlignedPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newAlignedPinnedPVar a
        fPtr <-
          maybe (error "Expected to get a Just ForeignPtr") pure $
          toForeignPtrPVar var
        withForeignPtr fPtr $ \ptr -> readPtr ptr `shouldReturn` a
    propPVarIO "copyPVarToPtr" gen $ \a var ->
      alloca $ \ptr -> do
        copyPVarToPtr var ptr
        readPtr ptr `shouldReturn` a
    prop "withNewPVarPtr" $
      forAllIO gen $ \a ->
        return $
        forAllIO gen $ \b ->
          withNewPtrPVar a $ \pvar ptr -> do
            sizeOfPVar pvar `shouldBe` Storable.sizeOf a
            a' <- readPtr ptr
            a' `shouldBe` a
            writePtr ptr b
            b' <- readPVar pvar
            b' `shouldBe` b


specAtomic :: (Show e, Eq e, Prim e, Num e, AtomicCount e, AtomicBits e) => Gen e -> Spec
specAtomic gen = do
  let basicAtomicFetchOldProp name atomicFun fun =
        propPVarIO name gen $ \x var ->
          return $
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` x
            atomicReadPVar var `shouldReturn` (x `fun` y)
      basicAtomicFetchNewProp name atomicFun fun =
        propPVarIO name gen $ \x var ->
          return $
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` (x `fun` y)
            atomicReadPVar var `shouldReturn` (x `fun` y)
  describe "Atomic (basic)" $ do
    describe "Basic" $ do
      basicAtomicFetchOldProp "atomicAddFetchOldPVar" atomicAddFetchOldPVar (+)
      basicAtomicFetchNewProp "atomicAddFetchNewPVar" atomicAddFetchNewPVar (+)
      basicAtomicFetchNewProp "atomicSubFetchOldPVar" atomicSubFetchOldPVar (-)
      basicAtomicFetchNewProp "atomicSubFetchNewPVar" atomicSubFetchNewPVar (-)
      propPVarIO "atomicAndFetchOldPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          atomicAndFetchOldPVar var y `shouldReturn` x
          atomicReadPVar var `shouldReturn` (x .&. y)
      propPVarIO "atomicNandFetchOldPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicNandFetchOldPVar var y
          x' `shouldBe` x
          atomicReadPVar var `shouldReturn` complement (x .&. y)
      propPVarIO "atomicOrFetchOldPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicOrFetchOldPVar var y
          x' `shouldBe` x
          atomicReadPVar var `shouldReturn` (x .|. y)
      propPVarIO "atomicXorFetchOldPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicXorFetchOldPVar var y
          x' `shouldBe` x
          atomicReadPVar var `shouldReturn` (x `xor` y)
      propPVarIO "atomicNotFetchOldPVar" gen $ \x var -> do
        x' <- atomicNotFetchOldPVar var
        x' `shouldBe` x
        atomicReadPVar var `shouldReturn` complement x
    describe "Concurrent" $ do
      propPVarIO "atomicAndFetchOldPVar" gen $ \x var ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicAndFetchOldPVar var) xs
          x' <- atomicReadPVar var
          F.foldl' (.&.) x' xs' `shouldBe` F.foldl' (.&.) x xs
      propPVarIO "atomicOrFetchOldPVar" gen $ \x var ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicOrFetchOldPVar var) xs
          x' <- atomicReadPVar var
          F.foldl' (.|.) x' xs' `shouldBe` F.foldl' (.|.) x xs
    describe "CAS-Concurrent" $ do
      propPVarIO "casPVar" gen $ \x var ->
        return $
        forAllIO ((,) <$> gen <*> gen) $ \(y, z) -> do
          x' <- casPVar var x y
          x' `shouldBe` x
          y' <- atomicReadPVar var
          atomicWritePVar var z
          y' `shouldBe` y
          z' <- atomicReadPVar var
          z' `shouldBe` z
      casProp_ "atomicAndFetchOldPVar" (+) atomicAndFetchOldPVar
      casProp_ "atomicSubFetchOldPVar" subtract atomicSubFetchOldPVar
      casProp "atomicAndFetchOldPVar" (.&.) atomicAndFetchOldPVar
      casProp "atomicOrFetchOldPVar" (.|.) atomicOrFetchOldPVar
      casProp_  "atomicXorFetchOldPVar" xor atomicXorFetchOldPVar
      propPVarIO "atomicNotPVar" gen $ \x xvar ->
        return $
        forAllIO arbitrary $ \(Positive n) -> do
          xs' <-
            mapConcurrently (\_ -> atomicNotFetchOldPVar xvar) [1 :: Int .. n]
          x' <- atomicReadPVar xvar
          yvar <- newPVar x
          ys' <-
            mapConcurrently
              (\_ -> atomicModifyFetchOldPVar yvar complement)
              [1 .. n]
          y' <- atomicReadPVar yvar
          x' `shouldBe` y'
          -- binary negation of N times results in two values, both of which happen N/2
          -- times
          let sxs@(l, r) = partition (== x) (x' : xs')
              lenr = length r
              sys = partition (== x) (y' : ys')
          sxs `shouldBe` sys
          length l `shouldSatisfy` (\len -> len == lenr || len == lenr + 1)
  where
    casProp_ name f af =
      propPVarIO name gen $ \x xvar ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          void $ mapConcurrently (af xvar) xs
          x' <- atomicReadPVar xvar
          yvar <- newPVar x
          void $ mapConcurrently (atomicModifyPVar_ yvar . f) xs
          y' <- atomicReadPVar yvar
          x' `shouldBe` y'
    casProp name f af =
      propPVarIO name gen $ \x xvar ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (af xvar) xs
          x' <- atomicReadPVar xvar
          yvar <- newPVar x
          ys' <-
            mapConcurrently (\y' -> atomicModifyFetchOldPVar yvar (`f` y')) xs
          y' <- atomicReadPVar yvar
          atomicWritePVar yvar x
          ys'' <-
            mapConcurrently (\y'' -> atomicModifyFetchOldPVar yvar (`f` y'')) xs
          x' `shouldBe` y'
          F.foldl' f x' xs' `shouldBe` F.foldl' f x xs
          F.foldl' f y' ys' `shouldBe` F.foldl' f x xs
          F.foldl' f x ys'' `shouldBe` F.foldl' f x xs

spec :: Spec
spec = do
  specPrim 0 (genValid :: Gen Int) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Int8) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Int16) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Int32) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Int64) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Word) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Word8) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Word16) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Word32) (\gen -> specStorable gen >> specAtomic gen)
  specPrim 0 (genValid :: Gen Word64) (\gen -> specStorable gen >> specAtomic gen)
  specPrim '\0' (genValid :: Gen Char) specStorable
  specPrim 0 (arbitrary :: Gen Float) specStorable
  specPrim 0 (arbitrary :: Gen Double) specStorable
