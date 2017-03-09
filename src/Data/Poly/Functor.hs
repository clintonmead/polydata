{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Poly.Functor (
  PolyFunctor, hmap, PolyFunctorConstraint
  )
where

import GHC.Exts (Constraint)
import Data.Poly (Poly(Poly))
import Control.IndexT (IndexT)
import Control.IndexT.Tuple (IsTuple)

type family PolyFunctorConstraint (c :: * -> Constraint) t :: Constraint

{-|
A very generic class for a map function on heterogeneous data structures (i.e. those with differing types).

This allows you to do things like:

>>> hmap triple (3 :: Int, 4.5 :: Float)
(9 :: Int, 13.5 :: Float)

'hmap' takes as it's function a 'Poly', as of course you'd want a polymorphic function.

The return type defined in the class is very vague, indeed it's just @t@ to be detailed in the instances,
because unlike a normal 'map' function, how 'hmap' changes the type depends a lot on the type it's applied to,
there's no simple @f a -> f b@.

Currently only instances are defined are for 2 and 3 tuples, nag me if you want larger ones.

It's worth noting how the instances are defined, for example, for the 2 tuples, there are 3 instances defined.
This is primarily to help type inference. We don't know too much about the types 'hmap' will produce, but we do know,
if we feed 'hmap' a pair, we should get a pair back. Likewise, if the result of 'hmap' is a pair, then the input
should be a pair.

So we provide both instances where the input is a pair, and when the output is a pair. In both of these instances,
we then in the constraints section (which happens after instance selection) ensure the other argument is also a pair.

The \"know both are pairs already\" case just needs to be added as a specific overlapping instance so the compiler
has a most specific match when it already knows both input and output are pairs.
-}

class PolyFunctor t where
  hmap :: forall c. PolyFunctorConstraint c t => Poly c -> t

type instance PolyFunctorConstraint c ((a0,a1) -> (b0,b1)) = (c (a0 -> b0), c (a1 -> b1))

instance (IsTuple 2 a) => PolyFunctor (a -> (b0,b1)) where
  hmap = hmapTuple2

instance (IsTuple 2 b) => PolyFunctor ((a0,a1) -> b) where
  hmap = hmapTuple2

instance {-# OVERLAPPING #-} PolyFunctor ((a0,a1) -> (b0,b1)) where
  hmap = hmapTuple2

hmapTuple2 (Poly f) (x, y) = (f x, f y)

type instance PolyFunctorConstraint c ((a0,a1,a2) -> (b0,b1,b2)) = (c (a0 -> b0), c (a1 -> b1), c (a2 -> b2))

instance (IsTuple 3 a) => PolyFunctor (a -> (b0,b1,b2)) where
  hmap = hmapTuple3

instance (IsTuple 3 b) => PolyFunctor ((a0,a1,a2) -> b) where
  hmap = hmapTuple3

instance {-# OVERLAPPING #-} PolyFunctor ((a0,a1,a2) -> (b0,b1,b2)) where
  hmap = hmapTuple3

hmapTuple3 (Poly f) (x, y, z) = (f x, f y, f z)
