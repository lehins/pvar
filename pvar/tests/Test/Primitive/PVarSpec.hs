{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Primitive.PVarSpec (spec) where

import Control.Monad
import Data.GenValidity
import Data.Int
import Data.Maybe
import Data.Primitive.ByteArray
import Data.Primitive.PVar
import Data.Primitive.PVar.Unsafe as Unsafe
import Data.Primitive.Types (sizeOf)
import Data.Typeable
import Data.Word
import Foreign.ForeignPtr
import qualified Foreign.Storable as Storable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
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
    propPVarIO "read" gen $ \v pvar -> readPVar pvar `shouldReturn` v
    propPVarIO "write/read" gen $ \_ pvar ->
      return $
      forAll gen $ \v -> do
        writePVar pvar v
        readPVar pvar `shouldReturn` v
    propPVarIO "newPinnedPVar" gen $ \a var -> do
      pinnedVar <- newPinnedPVar a
      (===) <$> readPVar var <*> readPVar pinnedVar
    propPVarIO "newAlignedPinnedPVar" gen $ \a var -> do
      pinnedVar <- newAlignedPinnedPVar a
      (===) <$> readPVar var <*> readPVar pinnedVar
    propPVarIO "modify" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar pvar (applyFun f) `shouldReturn` a
        readPVar pvar `shouldReturn` applyFun f a
    propPVarIO "modify_" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar_ pvar (applyFun f)
        readPVar pvar `shouldReturn` applyFun f a
    -- propPVarIO "modifyM" gen $ \a pvar ->
    --   return $
    --   forAll arbitrary $ \(NonEmptyList xs) -> do
    --     genM <- MWC.initialize $ V.fromList xs
    --     modifyPVar pvar $ \a' -> do
    --       a` shouldBe` a
    --       applyFun f) `shouldReturn` a
    --     readPVar pvar `shouldReturn` applyFun f a
    propPVarIO "swap" gen $ \a avar ->
      return $
      forAllPVarIO gen $ \b bvar -> do
        swapPVars avar bvar `shouldReturn` (a, b)
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propPVarIO "swap_" gen $ \a avar ->
      return $
      forAllPVarIO gen $ \b bvar -> do
        swapPVars_ avar bvar
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propPVarIO "copy" gen $ \a avar ->
      return $
      forAllPVarIO gen $ \_ bvar -> do
        copyPVar avar bvar
        readPVar bvar `shouldReturn` a
    propPVarST "sizeOf" gen $ \a avar -> pure (sizeOfPVar avar === sizeOf a)
    describe "Unsafe" $ do
      propPVarIO "copyPVarToMutableByteArray" gen $ \ a var ->
        return $
        forAll (genByteArrayNonEmpty gen) $ \(ByteArrayNonEmpty i ba) -> monadicIO $ run $ do
          mba <- unsafeThawByteArray ba
          copyPVarToMutableByteArray var mba i
          readByteArray mba i `shouldReturn` a
          (===) <$> readByteArray mba i <*> readPVar var
      propPVarIO "copyFromByteArrayPVar" gen $ \ _ var ->
        return $
        forAll (genByteArrayNonEmpty gen) $ \(ByteArrayNonEmpty i ba) -> monadicIO $ run $ do
          copyFromByteArrayPVar ba i var
          readPVar var `shouldReturn` indexByteArray ba i
      propPVarIO "copyFromMutableByteArrayPVar" gen $ \ _ var ->
        return $
        forAll (genByteArrayNonEmpty gen) $ \(ByteArrayNonEmpty i ba) -> monadicIO $ run $ do
          mba <- unsafeThawByteArray ba
          copyFromMutableByteArrayPVar mba i var
          readPVar var `shouldReturn` indexByteArray ba i
      propPVarST "sizeOf" gen $ \a var -> pure (toPtrPVar var === Nothing)
    describe "Num" $ do
      propPVarIO "zero" gen $ \_ var -> do
        zeroPVar var
        readPVar var `shouldReturn` defZero
    extraSpec gen

specStorable ::
     (Show p, Eq p, Prim p, Storable.Storable p, Arbitrary p, CoArbitrary p, Function p)
  => Gen p
  -> Spec
specStorable gen =
  describe "Storable" $ do
    propPVarIO "withPVarPtr (newPVar)" gen $ \a var ->
      withPtrPVar var pure `shouldReturn` Nothing
    prop "withPVarPtr (newPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newPinnedPVar a
        fmap fromJust $ withPtrPVar var $ \ptr ->
          Storable.peek ptr `shouldReturn` a
    prop "withPVarPtr (newAlignedPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newAlignedPinnedPVar a
        fmap fromJust $ withPtrPVar var $ \ptr ->
          Storable.peek ptr `shouldReturn` a
    propPVarIO "toForeignPtr (newPVar)" gen $ \a var ->
      toForeignPtrPVar var `shouldBe` Nothing
    prop "toForeignPtr (newPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newPinnedPVar a
        fPtr <-
          maybe (error "Expected to get a Just ForeignPtr") pure $
          toForeignPtrPVar var
        withForeignPtr fPtr $ \ptr -> Storable.peek ptr `shouldReturn` a
    prop "toForeignPtr (newAlignedPinnedPVar)" $
      forAllIO gen $ \a -> do
        var <- newAlignedPinnedPVar a
        fPtr <-
          maybe (error "Expected to get a Just ForeignPtr") pure $
          toForeignPtrPVar var
        withForeignPtr fPtr $ \ptr -> Storable.peek ptr `shouldReturn` a

-- TODO: atomic
-- specAtomic ::
--      (Num p, Show p, Eq p, Prim p, Typeable p, Arbitrary p, CoArbitrary p, Function p)
--   => Gen p
--   -> Spec
-- specAtomic gen =
--   describe "Num" $ do
--     propPVarIO "zero" gen $ \_ var -> do
--       zeroPVar var
--       readPVar var `shouldReturn` 0


spec :: Spec
spec = do
  let extraSpec gen = specStorable gen
  specPrim 0 (genValid :: Gen Int) extraSpec -- >> specAtomic
  specPrim 0 (genValid :: Gen Int8) extraSpec
  specPrim 0 (genValid :: Gen Int16) extraSpec
  specPrim 0 (genValid :: Gen Int32) extraSpec
  specPrim 0 (genValid :: Gen Int64) extraSpec
  specPrim 0 (genValid :: Gen Word) extraSpec
  specPrim 0 (genValid :: Gen Word8) extraSpec
  specPrim 0 (genValid :: Gen Word16) extraSpec
  specPrim 0 (genValid :: Gen Word32) extraSpec
  specPrim 0 (genValid :: Gen Word64) extraSpec
  specPrim '\0' (genValid :: Gen Char) extraSpec
  specPrim 0 (arbitrary :: Gen Float) extraSpec
  specPrim 0 (arbitrary :: Gen Double) extraSpec
