{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Poly
import Data.Poly.Functor
import Data.Poly.Function
import Control.IndexT.Constructor
import Control.IndexT.Function
import Control.IndexT

import Control.ConstraintManip
import Data.List (foldl')
import Data.Type.Equality (type (~~))

import Test.Hspec (hspec, it, shouldBe)

polyToInteger = Poly @((IsFunc 1) &&& ((Arg 0) `IxConstrainBy` Integral) &&& ((Result 1) `IxIs` Integer)) toInteger
triple = Poly @((IsHomoFunc 1) &&& ((Arg 0) `IxConstrainBy` Num)) (*3)
add = Poly @((IsHomoFunc 2) &&& ((Arg 0) `IxConstrainBy` Num)) (+)
eq = Poly @((IsHomoArgFunc 2) &&& ((Arg 0) `IxConstrainBy` Eq) &&& ((Result 2) `IxIs` Bool)) (==)

type FoldConstraint t = (
  IsFunc 3 t, -- A fold is a function of three args
  IndexT 1 t ~ ResultT 3 t, -- The second (i.e. arg 1) is equal to the result
  IsFunc 2 (IndexT 0 t), -- the first argument (i.e. the fold function) is a function of two args
  (IndexT 0 (IndexT 0 t)) ~ (ResultT 2 (IndexT 0 t)), -- the first argument of the function which is the first argument is the same as it's third
  IndexT 1 t ~ (IndexT 0 (IndexT 0 t)), -- also, the first argument of the function which is the first argument is the same as the second argument of the function
  IsData 1 (IndexT 2 t), -- the third argument is a data type with one variable
  Foldable (GetConstructor1 (IndexT 2 t)), -- the constructor of that third argument is Foldable
  IndexC 1 0 (IndexT 2 t) ~~ IndexT 1 (IndexT 0 t) -- the parameter to the constructor of Foldable is the same as the second argument of the fold function
  )

class (FoldConstraint t) => FoldConstraintC t
instance (FoldConstraint t) => FoldConstraintC t


pfoldl' = Poly @FoldConstraintC foldl'

polyToInteger' = mkPolyFunc1 @Integral @(Equal Integer) toInteger
triple' = mkPolyHomoFunc1 @Num (*3)
add' = mkPolyHomoFunc2 @Num (+)
eq' = mkPolyHomoArgFunc2 @Eq @(Equal Bool) (==)

polyFold :: Poly FoldConstraintC -> (Int, Float, String)
polyFold (Poly foldFunc) =
  (foldFunc (+) 0 [1,2,3], foldFunc (+) 0 [1.5,2.5,3.5], foldFunc (++) "" ["Hello", ", ", "World"])

main = hspec $ do
  it "mkPolyFunc1 test" $ getPoly polyToInteger (10 :: Int) `shouldBe` 10
  it "hmap triple test" $ hmap triple (6 :: Int, 4.5 :: Float) `shouldBe` (18, 13.5)
  it "add test" $ getPoly add' (6 :: Int) 5 `shouldBe` 11
  it "eq test" $ getPoly eq (6 :: Int) 6 `shouldBe` True
  it "mkPolyFunc1' test" $ getPoly polyToInteger' (10 :: Int) `shouldBe` 10
  it "hmap triple' test" $ hmap triple' (6 :: Int, 4.5 :: Float) `shouldBe` (18, 13.5)
  it "add' test" $ getPoly add' (6 :: Int) 5 `shouldBe` 11
  it "eq' test" $ getPoly eq' (6 :: Int) 6 `shouldBe` True
  it "polyFold test" $ polyFold pfoldl' `shouldBe` (6,7.5,"Hello, World")
