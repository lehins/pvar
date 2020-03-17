{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Primitive.PVarSpec (spec) where

import Control.Monad
import Control.Concurrent.Async
import Data.GenValidity
import Data.Int
import Data.Bits
import Data.List (partition)
import Data.Foldable as F
import Data.Maybe
import Data.Primitive.ByteArray
import Data.Primitive.PVar
import Data.Primitive.PVar.Unsafe as Unsafe
import Data.Primitive.Types (sizeOf, alignment)
import Data.Typeable
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import qualified Foreign.Storable as Storable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

forAllIO :: (Show p, Testable t) => Gen p -> (p -> IO t) -> Property
forAllIO g propM = forAll g $ \v -> monadicIO $ run $ propM v

forAllST :: (Show p, Testable t) => Gen p -> (forall s. p -> ST s t) -> Property
forAllST g propM = forAll g $ \v -> monadicST $ run $ propM v


forAllPVarST ::
     (Show p, Prim p, Testable t)
  => Gen p
  -> (forall s. p -> PVar s p -> ST s t)
  -> Property
forAllPVarST g propM = forAllST g $ \v -> newPVar v >>= propM v

forAllPVarIO ::
     (Show p, Prim p, Testable t)
  => Gen p
  -> (p -> PVar RealWorld p -> IO t)
  -> Property
forAllPVarIO g propM = forAllIO g $ \v -> newPVar v >>= propM v

propPVarST ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (forall s. p -> PVar s p -> ST s t)
  -> Spec
propPVarST name gen action = prop name $ forAllPVarST gen action

propPVarIO ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (p -> PVar RealWorld p -> IO t)
  -> Spec
propPVarIO name gen action = prop name $ forAllPVarIO gen action

-- | Generator for a non empty byte array that holds at least one element of type
-- @a@. Also contains a valid index in number of elements into the array
data ByteArrayNonEmpty a =
  ByteArrayNonEmpty Int ByteArray
  deriving (Show, Eq)

instance (Arbitrary a, Prim a) => Arbitrary (ByteArrayNonEmpty a) where
  arbitrary = genByteArrayNonEmpty (arbitrary :: Gen a)

genByteArrayNonEmpty gen = do
  Positive n <- arbitrary
  xs <- vectorOf n gen
  NonNegative i <- arbitrary
  pure $
    ByteArrayNonEmpty (i `mod` n) $
    runST $ do
      mba <- newByteArray (n * sizeOf (head xs))
      zipWithM_ (writeByteArray mba) [0 ..] xs
      unsafeFreezeByteArray mba

specPrim ::
     (Show p, Eq p, Prim p, Typeable p, Arbitrary p, CoArbitrary p, Function p)
  => p -- ^ Zero value
  -> Gen p
  -> (Gen p -> Spec)
  -> Spec
specPrim defZero gen extraSpec =
  describe ("PVar s " ++ showsType gen "") $ do
    propPVarIO "readPVar" gen $ \v pvar -> readPVar pvar `shouldReturn` v
    propPVarIO "writePVar/readPVar" gen $ \_ pvar ->
      return $
      forAll gen $ \v -> do
        writePVar pvar v
        readPVar pvar `shouldReturn` v
    prop "withPVarST" $
      forAll gen $ \a ->
        forAll gen $ \b ->
          withPVarST a $ \ var -> do
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
    propPVarIO "modifyPVar" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar pvar (applyFun f) `shouldReturn` a
        readPVar pvar `shouldReturn` applyFun f a
    propPVarIO "modifyPVar_" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar_ pvar (applyFun f)
        readPVar pvar `shouldReturn` applyFun f a
    propPVarIO "modifyPVarM" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        a' <-
          modifyPVarM pvar $ \a' -> do
            a' `shouldBe` a
            pure $ applyFun f a'
        a' `shouldBe` a
        readPVar pvar `shouldReturn` applyFun f a
    propPVarIO "modifyPVarM_" gen $ \a pvar ->
      return $
      forAllIO arbitrary $ \f -> do
        modifyPVarM_ pvar $ \a' -> do
          a' `shouldBe` a
          pure $ applyFun f a'
        readPVar pvar `shouldReturn` applyFun f a
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
    propPVarST "sizeOfPVar" gen $ \a avar -> pure (sizeOfPVar avar === sizeOf a)
    propPVarST "alignmentPVar" gen $ \a avar ->
      pure (alignmentPVar avar === alignment a)
    describe "Unsafe" $ do
      propPVarIO "copyPVarToMutableByteArray" gen $ \a var ->
        return $
        forAll (genByteArrayNonEmpty gen) $ \(ByteArrayNonEmpty i ba) ->
          monadicIO $
          run $ do
            mba <- unsafeThawByteArray ba
            copyPVarToMutableByteArray var mba i
            readByteArray mba i `shouldReturn` a
            (===) <$> readByteArray mba i <*> readPVar var
      propPVarIO "copyFromByteArrayPVar" gen $ \_ var ->
        return $
        forAll (genByteArrayNonEmpty gen) $ \(ByteArrayNonEmpty i ba) ->
          monadicIO $
          run $ do
            copyFromByteArrayPVar ba i var
            readPVar var `shouldReturn` indexByteArray ba i
      propPVarIO "copyFromMutableByteArrayPVar" gen $ \_ var ->
        return $
        forAll (genByteArrayNonEmpty gen) $ \(ByteArrayNonEmpty i ba) ->
          monadicIO $
          run $ do
            mba <- unsafeThawByteArray ba
            copyFromMutableByteArrayPVar mba i var
            readPVar var `shouldReturn` indexByteArray ba i
      propPVarST "sizeOf" gen $ \a var -> pure (toPtrPVar var === Nothing)
    describe "Reset Memory" $
      propPVarIO "zeroPVar" gen $ \_ var -> do
        zeroPVar var
        readPVar var `shouldReturn` defZero
    extraSpec gen

specStorable ::
     (Show p, Eq p, Prim p, Storable p, Arbitrary p, CoArbitrary p, Function p)
  => Gen p
  -> Spec
specStorable gen =
  describe "Storable" $ do
    propPVarIO "withPVarPtr (newPVar)" gen $ \a var ->
      withPtrPVar var pure `shouldReturn` Nothing
    prop "withPVarPtr (newPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newPinnedPVar a
        fmap fromJust $
          withPtrPVar var $ \ptr -> peek ptr `shouldReturn` a
    prop "withPVarPtr (newAlignedPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newAlignedPinnedPVar a
        fmap fromJust $
          withPtrPVar var $ \ptr -> peek ptr `shouldReturn` a
    propPVarIO "toForeignPtr (newPVar)" gen $ \a var ->
      toForeignPtrPVar var `shouldBe` Nothing
    prop "toForeignPtr (newPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newPinnedPVar a
        fPtr <-
          maybe (error "Expected to get a Just ForeignPtr") pure $
          toForeignPtrPVar var
        withForeignPtr fPtr $ \ptr -> peek ptr `shouldReturn` a
    prop "toForeignPtr (newAlignedPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newAlignedPinnedPVar a
        fPtr <-
          maybe (error "Expected to get a Just ForeignPtr") pure $
          toForeignPtrPVar var
        withForeignPtr fPtr $ \ptr -> peek ptr `shouldReturn` a
    propPVarIO "poke/peek/ (Ptr PVar)" gen $ \a var ->
      alloca $ \ptr -> do
        Storable.poke ptr var
        var' <- Storable.peek ptr
        readPVar var' `shouldReturn` a
        Storable.sizeOf var `shouldBe` sizeOfPVar var
        Storable.alignment var `shouldBe` alignmentPVar var
    propPVarIO "copyPVarToPtr" gen $ \a var ->
      alloca $ \ptr -> do
        copyPVarToPtr var ptr
        peek ptr `shouldReturn` a

specAtomic :: Spec
specAtomic = do
  let gen = genValid :: Gen Int
  describe "Atomic (basic)" $ do
    describe "Basic" $ do
      propPVarIO "atomicAddIntPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicAddIntPVar var y
          x' `shouldBe` x
          atomicReadIntPVar var `shouldReturn` (x + y)
      propPVarIO "atomicSubIntPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicSubIntPVar var y
          x' `shouldBe` x
          atomicReadIntPVar var `shouldReturn` (x - y)
      propPVarIO "atomicAndIntPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicAndIntPVar var y
          x' `shouldBe` x
          atomicReadIntPVar var `shouldReturn` (x .&. y)
      propPVarIO "atomicNandIntPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicNandIntPVar var y
          x' `shouldBe` x
          atomicReadIntPVar var `shouldReturn` complement (x .&. y)
      propPVarIO "atomicOrIntPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicOrIntPVar var y
          x' `shouldBe` x
          atomicReadIntPVar var `shouldReturn` (x .|. y)
      propPVarIO "atomicXorIntPVar" gen $ \x var ->
        return $
        forAllIO gen $ \y -> do
          x' <- atomicXorIntPVar var y
          x' `shouldBe` x
          atomicReadIntPVar var `shouldReturn` (x `xor` y)
      propPVarIO "atomicNotIntPVar" gen $ \x var -> do
        x' <- atomicNotIntPVar var
        x' `shouldBe` x
        atomicReadIntPVar var `shouldReturn` complement x
    describe "Concurrent" $ do
      propPVarIO "atomicAndIntPVar" gen $ \x var ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicAndIntPVar var) xs
          x' <- atomicReadIntPVar var
          F.foldl' (.&.) x' xs' `shouldBe` F.foldl' (.&.) x xs
      propPVarIO "atomicOrIntPVar" gen $ \x var ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (atomicOrIntPVar var) xs
          x' <- atomicReadIntPVar var
          F.foldl' (.|.) x' xs' `shouldBe` F.foldl' (.|.) x xs
    describe "CAS-Concurrent" $ do
      propPVarIO "casIntPVar" gen $ \x var ->
        return $
        forAllIO ((,) <$> gen <*> gen) $ \(y, z) -> do
          x' <- casIntPVar var x y
          x' `shouldBe` x
          y' <- atomicReadIntPVar var
          atomicWriteIntPVar var z
          y' `shouldBe` y
          z' <- atomicReadIntPVar var
          z' `shouldBe` z
      casProp_ gen "atomicAddIntPVar" (+) atomicAddIntPVar
      casProp_ gen "atomicSubIntPVar" subtract atomicSubIntPVar
      casProp gen "atomicAndIntPVar" (.&.) atomicAndIntPVar
      casProp gen "atomicOrIntPVar" (.|.) atomicOrIntPVar
      casProp_ gen "atomicXorIntPVar" xor atomicXorIntPVar
      propPVarIO "atomicNotIntPVar" gen $ \x xvar ->
        return $
        forAllIO arbitrary $ \(Positive n) -> do
          xs' <- replicateConcurrently n (atomicNotIntPVar xvar)
          x' <- atomicReadIntPVar xvar
          yvar <- newPVar x
          ys' <-
            replicateConcurrently
              n
              (atomicModifyIntPVar yvar (\y -> (complement y, y)))
          y' <- atomicReadIntPVar yvar
          x' `shouldBe` y'
          -- binary negation of N times results in two values, both of which happen N/2
          -- times
          let sxs@(l, r) = partition (== x) (x' : xs')
              lenr = length r
              sys = partition (== x) (y' : ys')
          sxs `shouldBe` sys
          length l `shouldSatisfy` (\len -> len == lenr || len == lenr + 1)
  where
    casProp_ gen name f af =
      propPVarIO name gen $ \x xvar ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          mapConcurrently_ (af xvar) xs
          x' <- atomicReadIntPVar xvar
          yvar <- newPVar x
          mapConcurrently_ (\y' -> atomicModifyIntPVar_ yvar (f y')) xs
          y' <- atomicReadIntPVar yvar
          x' `shouldBe` y'
    casProp gen name f af =
      propPVarIO name gen $ \x xvar ->
        return $
        forAllIO (genListOf gen) $ \xs -> do
          xs' <- mapConcurrently (af xvar) xs
          x' <- atomicReadIntPVar xvar
          yvar <- newPVar x
          ys' <-
            mapConcurrently
              (\y' -> atomicModifyIntPVar yvar (\y -> (f y y', y)))
              xs
          y' <- atomicReadIntPVar yvar
          x' `shouldBe` y'
          F.foldl' f x' xs' `shouldBe` F.foldl' f x xs


spec :: Spec
spec = do
  specPrim 0 (genValid :: Gen Int) (\gen -> specStorable gen >> specAtomic)
  specPrim 0 (genValid :: Gen Int8) specStorable
  specPrim 0 (genValid :: Gen Int16) specStorable
  specPrim 0 (genValid :: Gen Int32) specStorable
  specPrim 0 (genValid :: Gen Int64) specStorable
  specPrim 0 (genValid :: Gen Word) specStorable
  specPrim 0 (genValid :: Gen Word8) specStorable
  specPrim 0 (genValid :: Gen Word16) specStorable
  specPrim 0 (genValid :: Gen Word32) specStorable
  specPrim 0 (genValid :: Gen Word64) specStorable
  specPrim '\0' (genValid :: Gen Char) specStorable
  specPrim 0 (arbitrary :: Gen Float) specStorable
  specPrim 0 (arbitrary :: Gen Double) specStorable
