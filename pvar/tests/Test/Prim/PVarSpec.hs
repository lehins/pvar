{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Prim.PVarSpec (spec) where

import Control.Concurrent.Async
import Control.Prim.Monad
import Control.Prim.Eval
import Data.Bits
import Data.Foldable as F
import Data.GenValidity
import Data.List (partition)
import Data.Maybe
import Data.Prim
import Data.Prim.PVar
import Data.Prim.PVar.Unsafe as Unsafe
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

propPVar ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (p -> PVar p RealWorld -> t)
  -> Spec
propPVar name gen action = prop name $ forAllPVarIO gen (\p -> return . action p)

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
    propPVar "readPVar" gen $ \v pvar -- deepseq is used for coverage only
     -> pvar `deepseq` readPVar pvar `shouldReturn` v
    propPVar "writePVar/readPVar" gen $ \_ pvar ->
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
    propPVarST "newPinnedPVar" gen $ \a var -> do
      pinnedVar <- newPinnedPVar a
      (===) <$> readPVar var <*> readPVar pinnedVar
    propPVarST "newAlignedPinnedPVar" gen $ \a var -> do
      pinnedVar <- newAlignedPinnedPVar a
      (===) <$> readPVar var <*> readPVar pinnedVar
    propPVar "modifyPVar_" gen $ \a pvar ->
      forAll arbitrary $ \f -> do
        modifyPVar_ pvar (apply f)
        readPVar pvar `shouldReturn` apply f a
    propPVar "modifyPVar" gen $ \a pvar ->
      forAll arbitrary $ \f -> do
        let (a', b :: Int) = apply f a
        modifyPVar pvar (apply f) `shouldReturn` b
        readPVar pvar `shouldReturn` a'
    propPVar "modifyFetchOldPVar" gen $ \a pvar ->
      forAll arbitrary $ \f -> do
        modifyFetchOldPVar pvar (apply f) `shouldReturn` a
        readPVar pvar `shouldReturn` apply f a
    propPVar "modifyFetchOldPVarM" gen $ \a pvar ->
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyFetchOldPVarM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ apply f a'
        a' `shouldBe` a
        readPVar pvar `shouldReturn` apply f a
    propPVar "modifyFetchNewPVar" gen $ \a pvar ->
      forAll arbitrary $ \f ->
        modifyFetchNewPVar pvar (apply f) `shouldReturn` apply f a
    propPVar "modifyFetchNewPVarM" gen $ \a pvar ->
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyFetchNewPVarM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ apply f a'
        a' `shouldBe` apply f a
    propPVar "modifyPVarM_" gen $ \a pvar ->
      forAllIO arbitrary $ \f -> do
        modifyPVarM_ pvar $ \a' -> do
          a' `shouldBe` a
          pure $ apply f a'
        readPVar pvar `shouldReturn` apply f a
    propPVar "swapPVars" gen $ \a avar ->
      forAllPVarIO gen $ \b bvar -> do
        swapPVars avar bvar `shouldReturn` (a, b)
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propPVar "swapPVars_" gen $ \a avar ->
      forAllPVarIO gen $ \b bvar -> do
        swapPVars_ avar bvar
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propPVar "copyPVar" gen $ \a avar ->
      forAllPVarIO gen $ \_ bvar -> do
        copyPVar avar bvar
        readPVar bvar `shouldReturn` a
    propPVarST "sizeOfPVar" gen $ \a avar -> pure (sizeOfPVar avar === Storable.sizeOf a)
    propPVarST "alignmentPVar" gen $ \a avar ->
      pure (alignmentPVar avar === alignment a)
    describe "Unsafe" $ do
      propPVarST "sizeOf" gen $ \_ var -> pure (toPtrPVar var === Nothing)
    describe "Reset Memory" $
      propPVar "zeroPVar" gen $ \_ var -> do
        zeroPVar var
        readPVar var `shouldReturn` defZero
    extraSpec gen

specStorable ::
     (Show p, Eq p, Prim p, Storable.Storable p, Arbitrary p, CoArbitrary p, Function p)
  => Gen p
  -> Spec
specStorable gen =
  describe "Storable" $ do
    propPVar "withPVarPtr (newPVar)" gen $ \_ var ->
      withPtrPVar var pure `shouldReturn` Nothing
    prop "unsafeToForeignPtr" $
      forAllIO gen $ \x -> do
        pvar <- newPinnedPVar x
        withForeignPtr (Unsafe.unsafeToForeignPtrPVar pvar) $ \ptr ->
          Storable.peek ptr `shouldReturn` x
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
    propPVar "toForeignPtr (newPVar)" gen $ \_ var ->
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
    propPVar "copyPVarToPtr" gen $ \a var ->
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


specAtomic ::
     forall e.
     ( Show e
     , Eq e
     , Prim e
     , Num e
     , AtomicCount e
     , AtomicBits e
     )
  => Gen e
  -> Spec
specAtomic gen = do
  let basicAtomicProp name atomicFun fun =
        propPVar name gen $ \x var ->
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` ()
            atomicReadPVar var `shouldReturn` (x `fun` y)
      basicAtomicFetchOldProp name atomicFun fun =
        propPVar name gen $ \x var ->
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` x
            atomicReadPVar var `shouldReturn` (x `fun` y)
      basicAtomicFetchNewProp name atomicFun fun =
        propPVar name gen $ \x var ->
          forAllIO gen $ \y -> do
            atomicFun var y `shouldReturn` (x `fun` y)
            atomicReadPVar var `shouldReturn` (x `fun` y)
  describe "Atomic (basic)" $ do
    describe "Basic" $ do
      basicAtomicProp "atomicAddFetch_" atomicAddPVar_ (+)
      basicAtomicFetchOldProp "atomicAddFetchOldPVar" atomicAddFetchOldPVar (+)
      basicAtomicFetchNewProp "atomicAddFetchNewPVar" atomicAddFetchNewPVar (+)
      basicAtomicProp "atomicSubFetch_" atomicSubPVar_ (-)
      basicAtomicFetchOldProp "atomicSubFetchOldPVar" atomicSubFetchOldPVar (-)
      basicAtomicFetchNewProp "atomicSubFetchNewPVar" atomicSubFetchNewPVar (-)
      basicAtomicProp "atomicAndFetch_" atomicAndPVar_ (.&.)
      basicAtomicFetchOldProp "atomicAndFetchOldPVar" atomicAndFetchOldPVar (.&.)
      basicAtomicFetchNewProp "atomicAndFetchNewPVar" atomicAndFetchNewPVar (.&.)
      let nand x y = complement (x .&. y)
      basicAtomicProp "atomicNandFetch_" atomicNandPVar_ nand
      basicAtomicFetchOldProp "atomicNandFetchOldPVar" atomicNandFetchOldPVar nand
      basicAtomicFetchNewProp "atomicNandFetchNewPVar" atomicNandFetchNewPVar nand
      basicAtomicProp "atomicOrFetch_" atomicOrPVar_ (.|.)
      basicAtomicFetchOldProp "atomicOrFetchOldPVar" atomicOrFetchOldPVar (.|.)
      basicAtomicFetchNewProp "atomicOrFetchNewPVar" atomicOrFetchNewPVar (.|.)
      basicAtomicProp "atomicXorFetch_" atomicXorPVar_ xor
      basicAtomicFetchOldProp "atomicXorFetchOldPVar" atomicXorFetchOldPVar xor
      basicAtomicFetchNewProp "atomicXorFetchNewPVar" atomicXorFetchNewPVar xor
      propPVar "atomicNotPVar_" gen $ \x var -> do
        atomicNotPVar_ var
        atomicReadPVar var `shouldReturn` complement x
      propPVar "atomicNotFetchOldPVar" gen $ \x var -> do
        atomicNotFetchOldPVar var `shouldReturn` x
        atomicReadPVar var `shouldReturn` complement x
      propPVar "atomicNotFetchNewPVar" gen $ \x var -> do
        atomicNotFetchNewPVar var `shouldReturn` complement x
        atomicReadPVar var `shouldReturn` complement x
    describe "Concurrent" $ do
      propPVar "atomicAndFetchOldPVar" gen $ \x var ->
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicAndFetchOldPVar var) xs
          x' <- atomicReadPVar var
          F.foldl' (.&.) x' xs' `shouldBe` F.foldl' (.&.) x xs
      propPVar "atomicOrFetchOldPVar" gen $ \x var ->
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicOrFetchOldPVar var) xs
          x' <- atomicReadPVar var
          F.foldl' (.|.) x' xs' `shouldBe` F.foldl' (.|.) x xs
    describe "CAS-Concurrent" $ do
      propPVar "casPVar" gen $ \x var ->
        forAllIO ((,) <$> gen <*> gen) $ \(y, z) -> do
          casPVar var x y `shouldReturn` x
          atomicReadPVar var `shouldReturn` y
          atomicWritePVar var z
          atomicReadPVar var `shouldReturn` z
      casProp_ "atomicAddFetchOldPVar" (+) atomicAddFetchOldPVar
      casProp_ "atomicSubFetchOldPVar" subtract atomicSubFetchOldPVar
      casSymAssocProp "atomicAndFetchOldPVar" (.&.) atomicAndFetchOldPVar
      casSymAssocProp "atomicOrFetchOldPVar" (.|.) atomicOrFetchOldPVar
      casProp_ "atomicXorFetchOldPVar" xor atomicXorFetchOldPVar
      propPVar "atomicNotPVar" gen $ \x xvar ->
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
      prop "atomicModifyPVar" $
        forAll gen $ \z ->
          forAllIO (listOf gen) $ \xs -> do
            zvar <- newPVar $ Atom (Nothing, z)
            -- non-associative function that will produce different results depending on
            -- the order of application to the list of xs
            let g = (-)
            mxs <-
              mapConcurrently
                (\x ->
                   atomicModifyPVar
                     zvar
                     (\(Atom (mPrev, a)) -> (Atom (Just x, g a x), mPrev)))
                xs
            Atom (mLast, z') <- atomicReadPVar zvar
            let xs' = catMaybes (mxs ++ [mLast])
            xs' `shouldMatchList` xs
            z' `shouldBe` F.foldl' g z xs'
  where
    casProp_ name f af =
      propPVar name gen $ \x xvar ->
        forAllIO (genListOf gen) $ \xs -> do
          void $ mapConcurrently (af xvar) xs
          x' <- atomicReadPVar xvar
          yvar <- newPVar x
          void $ mapConcurrently (atomicModifyPVar_ yvar . f) xs
          atomicReadPVar yvar `shouldReturn` x'
    casSymAssocProp name f af =
      propPVar name gen $ \x xvar ->
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (af xvar) xs
          x' <- atomicReadPVar xvar
          yvar <- newPVar x
          ys' <-
            mapConcurrently (\a -> atomicModifyFetchOldPVar yvar (`f` a)) xs
          atomicReadPVar yvar `shouldReturn` x'
          atomicWritePVar yvar x
          ys'' <-
            mapConcurrently (\a -> atomicModifyFetchNewPVar yvar (`f` a)) xs
          atomicReadPVar yvar `shouldReturn` x'
          let res = F.foldl' f x xs
          F.foldl' f x' xs' `shouldBe` res
          F.foldl' f x' ys' `shouldBe` res
          F.foldl' f x ys'' `shouldBe` res

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
