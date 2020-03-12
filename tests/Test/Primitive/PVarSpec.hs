{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Primitive.PVarSpec (spec) where

import Data.Primitive.PVar
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Typeable
import Data.Word
import Data.Int
import Data.GenValidity
import Data.Primitive.Types (sizeOf)

forAllST ::
     (Show p, Prim p, Testable t)
  => Gen p
  -> (forall s. p -> PVar s p -> ST s t)
  -> Property
forAllST g propM = forAll g $ \v -> monadicST $ run $ newPVar v >>= propM v

forAllIO ::
     (Show p, Prim p, Testable t)
  => Gen p
  -> (p -> PVar RealWorld p -> IO t)
  -> Property
forAllIO g propM = forAll g $ \v -> monadicIO $ run $ newPVar v >>= propM v

propST ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (forall s. p -> PVar s p -> ST s t)
  -> Spec
propST name gen action = prop name $ forAllST gen action

propIO ::
     (Show p, Prim p, Testable t)
  => String
  -> Gen p
  -> (p -> PVar RealWorld p -> IO t)
  -> Spec
propIO name gen action = prop name $ forAllIO gen action

showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep


specPrim ::
     (Show p, Eq p, Prim p, Typeable p, Arbitrary p, CoArbitrary p, Function p)
  => Gen p
  -> Spec
specPrim gen =
  describe ("PVar s " ++ showsType gen "") $ do
    propIO "read" gen $ \v pvar -> readPVar pvar `shouldReturn` v
    propIO "write/read" gen $ \_ pvar ->
      return $
      forAll gen $ \v -> do
        writePVar pvar v
        readPVar pvar `shouldReturn` v
    propIO "modify" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar pvar (applyFun f) `shouldReturn` a
        readPVar pvar `shouldReturn` applyFun f a
    propIO "modify_" gen $ \a pvar ->
      return $
      forAll arbitrary $ \f -> do
        modifyPVar_ pvar (applyFun f)
        readPVar pvar `shouldReturn` applyFun f a
    -- propIO "modifyM" gen $ \a pvar ->
    --   return $
    --   forAll arbitrary $ \(NonEmptyList xs) -> do
    --     genM <- MWC.initialize $ V.fromList xs
    --     modifyPVar pvar $ \a' -> do
    --       a` shouldBe` a
    --       applyFun f) `shouldReturn` a
    --     readPVar pvar `shouldReturn` applyFun f a
    propIO "swap" gen $ \a avar ->
      return $
      forAllIO gen $ \b bvar -> do
        swapPVars avar bvar `shouldReturn` (a, b)
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propIO "swap_" gen $ \a avar ->
      return $
      forAllIO gen $ \b bvar -> do
        swapPVars_ avar bvar
        readPVar avar `shouldReturn` b
        readPVar bvar `shouldReturn` a
    propIO "copy" gen $ \a avar ->
      return $
      forAllIO gen $ \_ bvar -> do
        copyPVar avar bvar
        readPVar bvar `shouldReturn` a
    propST "sizeOf" gen $ \a avar -> pure (sizeOfPVar avar === sizeOf a)


spec :: Spec
spec = do
  specPrim (genValid :: Gen Int)
  specPrim (genValid :: Gen Int8)
  specPrim (genValid :: Gen Int16)
  specPrim (genValid :: Gen Int32)
  specPrim (genValid :: Gen Int64)
  specPrim (genValid :: Gen Word)
  specPrim (genValid :: Gen Word8)
  specPrim (genValid :: Gen Word16)
  specPrim (genValid :: Gen Word32)
  specPrim (genValid :: Gen Word64)
  specPrim (genValid :: Gen Char)
  specPrim (arbitrary :: Gen Float)
  specPrim (arbitrary :: Gen Double)
