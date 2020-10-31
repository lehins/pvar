{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Prim.Eval
import Control.Prim.Monad
import Criterion.Main
import Data.Atomics
import Data.IORef as Base
import Data.Int
import Data.Prim.Ref
--import Data.Prim.Atomic
import Data.Prim.PVar
import Prelude as P
import UnliftIO.Async
import UnliftIO.Concurrent

benchConcN ::
     (Num a, Enum a)
  => Int
  -> a
  -> IO ref
  -> String
  -> (ref -> a -> IO b)
  -> Benchmark
benchConcN c n mkEnv name f =
  env (BNF <$> mkEnv) $ \(BNF ref) ->
    bench name $ whnfIO $ pooledForConcurrentlyN_ c [1 .. n] (f ref)


main :: IO ()
main = do
  c <- getNumCapabilities
  let !k = 17 :: Int
      !n = 100000 :: Int
      !e0 = 16 :: Int
      tup x = (x, x)
      addTup k' (x, y) =
        let a'@(!_, !_) = (x + k', y - k')
         in a'
      mkPVar :: Prim e => (Int -> e) -> IO (PVar e RW)
      mkPVar f = newPVar (f e0)
      mkRef :: (Int -> e) -> IO (Ref e RW)
      mkRef f = newRef (f e0)
      mkIORef :: (Int -> e) -> IO (Base.IORef e)
      mkIORef f = newIORef (f e0)
      withIORef :: e -> (Base.IORef e -> Benchmark) -> Benchmark
      withIORef e g = env (BNF <$> newIORef e) $ \ref -> g (coerce ref)
      benchSeq mkEnv name f =
        env (BNF <$> mkEnv) $ \(BNF ref) ->
          bench name $ whnfIO $ forM_ [1 .. n] (f ref)
      benchConc mkEnv name f =
        env (BNF <$> mkEnv) $ \(BNF ref) ->
          bench name $ nfIO $ pooledForConcurrentlyN_ c [1 .. n] (f ref)
  defaultMain
    [ bgroup
        "Single"
        [ bgroup
            "Write"
            [ env (newPVar e0) $ \ref ->
                bgroup
                  "PVar"
                  [ bench "writePVar" $ nfIO (writePVar ref 1)
                  , bench "atomicWritePVar" $ nfIO (atomicWritePVar ref 2)
                  ]
            , withIORef e0 $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "writeIORef" $ nfIO (writeIORef ioRef 1)
                  , bench "atomicWriteIORef" $ nfIO (atomicWriteIORef ioRef 2)
                  ]
            ]
        , bgroup
            "AddFetchOld"
            [ env (newPVar 0) $ \ref ->
                bgroup
                  "PVar"
                  [ bench "modifyFetchOldPVar" $
                    nfIO $ modifyFetchOldPVar ref (+ k)
                  , bench "atomicModifyFetchOldPVar" $
                    nfIO $ atomicModifyFetchOldPVar ref (+ k)
                  , bench "atomicAddFetchOldPVar" $
                    nfIO $ atomicAddFetchOldPVar ref k
                  ]
            , withIORef 0 $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "modifyFetchOldIORef" $
                    nfIO $ do
                      a <- readIORef ioRef
                      let a' = a + k
                      a' `seq` (a <$ writeIORef ioRef a')
                  , bench "atomicModifyIORef'" $
                    nfIO $ atomicModifyIORef' ioRef (\x -> (x + k, x))
                  , bench "atomicModifyIORefCAS" $
                    nfIO $ atomicModifyIORefCAS ioRef (\x -> (x + k, x))
                  ]
            ]
        , bgroup
            "AddMaybe"
            [ bgroup
                "PVar"
                [ env (newPVar (Just 0)) $ \ref ->
                    bench "modifyFetchOldPVar" $
                    nfIO $ modifyFetchOldPVar ref (fmap (+ k))
                , env (newPVar (Atom (Just 0))) $ \ref ->
                    bench "atomicModifyFetchOldPVar" $
                    nfIO $
                    atomicModifyFetchOldPVar
                      ref
                      (\(Atom ma) -> coerce (fmap (+ k) ma))
                ]
            , withIORef (Just 0) $ \ioRef ->
                bgroup
                  "IORef"
                  [ bench "modifyFetchOldIORef" $
                    nfIO $ do
                      a <- readIORef ioRef
                      let a' = (k +) <$> a
                      a' `seq` (a <$ writeIORef ioRef a')
                  , bench "atomicModifyIORef'" $
                    nfIO $ atomicModifyIORef' ioRef (\x -> ((k +) <$> x, x))
                  , bench "atomicModifyIORefCAS" $
                    nfIO $ atomicModifyIORefCAS ioRef (\x -> ((k +) <$> x, x))
                  ]
            ]
        ]
    , bgroup
        "Sequential"
        [ bgroup
            "AddFetchOld"
            [ bgroup
                "PVar"
                [ benchSeq (mkPVar id) "modifyFetchOldPVar  (Int)" $ \ref k' ->
                    modifyFetchOldPVar ref (+ k')
                , benchSeq (mkPVar tup) "modifyFetchOldPVar (Int, Int)" $ \ref k' -> do
                    modifyFetchOldPVar ref (addTup k')
                ]
            , bgroup
                "IORef"
                [ benchSeq (mkIORef id) "modifyFetchOldIORef (Int)" $ \ioRef k' -> do
                    a <- readIORef ioRef
                    writeIORef ioRef $! a + k'
                    pure a
                , benchSeq (mkIORef tup) "modifyFetchOldIORef (Int, Int)" $ \ioRef k' -> do
                    a <- readIORef ioRef
                    writeIORef ioRef $! addTup k' a
                    pure a
                ]
            ]
        ]
    , bgroup
        "Concurrent"
        [ bgroup
            "AddFetchOld"
            [ bgroup
                "PVar"
                [ benchConcN c n (mkPVar id) "atomicModifyFetchOldPVar" $ \mb k' ->
                    atomicModifyFetchOldPVar mb (+ k')
                , benchConcN c n (mkPVar id) "atomicAddFetchOldPVar" $ \mb ->
                    atomicAddFetchOldPVar mb
                ]
            , bgroup
                "PVar (Int32)"
                [ benchConcN
                    c
                    (fromIntegral n)
                    (mkPVar (fromIntegral :: Int -> Int32))
                    "atomicModifyFetchOldPVar" $ \mb k' ->
                    atomicModifyFetchOldPVar mb (+ k')
                , benchConcN
                    c
                    (fromIntegral n)
                    (mkPVar (fromIntegral :: Int -> Int32))
                    "atomicAddFetchOldPVar" $ \mb -> atomicAddFetchOldPVar mb
                ]
            , bgroup
                "IORef"
                [ benchConc (mkRef id) "atomicModifyRef" $ \ioRef k' ->
                    atomicModifyRef ioRef (\x -> (x + k', x))
                , benchConc (mkIORef id) "atomicModifyIORef'" $ \ioRef k' ->
                    atomicModifyIORef' ioRef (\x -> (x + k', x))
                , benchConc (mkIORef id) "atomicModifyIORefCAS" $ \ioRef k' ->
                    atomicModifyIORefCAS ioRef (\x -> (x + k', x))
                ]
            ]
        ]
    , bgroup
        "TupleConcurrent"
        [ bgroup
            "AddFetchOld"
            [ bgroup
                "PVar"
                [ benchConc (mkPVar (Atom . tup)) "atomicModifyFetchOldPVar" $ \mb k' ->
                    atomicModifyFetchOldPVar
                      mb
                      (\(Atom a) -> Atom (addTup k' a))
                ]
            , bgroup
                "IORef"
                [ benchConc (mkRef tup) "atomicModifyRef" $ \ioRef k' ->
                    atomicModifyRef ioRef (\a -> (addTup k' a, a))
                , benchConc (mkIORef tup) "atomicModifyIORef'" $ \ioRef k' ->
                    atomicModifyIORef' ioRef (\a -> (addTup k' a, a))
                , benchConc (mkIORef tup) "atomicModifyIORefCAS" $ \ioRef k' ->
                    atomicModifyIORefCAS ioRef (\a -> (addTup k' a, a))
                ]
            ]
        ]
    ]
