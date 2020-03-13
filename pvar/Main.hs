module Main where

import Data.Primitive.PVar

main :: IO ()
main = do
  -- xvar <- newPVar 5
  -- x <- atomicModifyIntPVar_ xvar (+1)
  -- print x
  yvar <- newPVar 5
  y <- atomicModifyIntPVar yvar (\i -> (i + 1, i))
  print y
