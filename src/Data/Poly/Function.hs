{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Poly.Function (
  -- $mkPolyDoc
  mkPolyHomoFunc1,
  mkPolyFunc1,
  mkPolyHomoFunc2,
  mkPolyHomoArgFunc2,
  -- $convinienceConstraintDocs
  Equal,
  Empty
  )

where

import Data.Poly (Poly(Poly))
import Control.IndexT (IndexT)
import Control.IndexT.Function (
  ResultT,
  IsFunc,
  IsHomoFunc,
  IsHomoArgFunc
  )

import Control.ConstraintManip (
  type (&&&),
  Arg, Result,
  IxConstrainBy
  )

{- $mkPolyDoc
The easiest way to use these mkPoly* functions is by adding the extension:

> \{\-# LANGUAGE TypeApplications #\-\}

at the top of your source file (<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#visible-type-application GHC Documentation on the Type Application extension>).

Examples of this will be included in the documentation for each convience function.

Note that all of these convience functions are just type restricted versions of 'Poly', that's all,
and are all defined in this form:

> f = Poly

Also not that this is far from an exhaustive list of what can be done, there's a more general approach
described in the documentation for `Poly'
-}

{-|
'mkPolyHomoFunc1 simply represents a function from @t -> t@, possibly constrained.

For example, this is how to write a polymorphic version of "triple":

> mkPolyHomoFunc1 @Num (*3)

-}
mkPolyHomoFunc1 :: forall c. (forall t. c t => t -> t) -> Poly ((IsHomoFunc 1) &&& ((Arg 0) `IxConstrainBy` c))
mkPolyHomoFunc1 = Poly

{-|
'mkPolyFunc1 is for one argument functions with differing arguments.

For example, this is how to write a polymorphic version of 'toInteger':

> mkPolyFunc1 @Integral @(Equal Integer) toInteger

Note that something like @Just :: t -> Maybe t@ this convience function is not helpful for,
because the two constraints you pass here are separate.
-}
mkPolyFunc1 :: forall c1 c2. (forall t1 t2. (c1 t1, c2 t2) => t1 -> t2) -> Poly ((IsFunc 1) &&& ((Arg 0) `IxConstrainBy` c1) &&& ((Result 1) `IxConstrainBy` c2))
mkPolyFunc1 = Poly

{-|
'mkPolyHomoFunc2 simply represents a function from @t -> t -> t@, possibly constrained.

For example, this is how to write a polymorphic version of "add":

> mkPolyHomoFunc2 @Num (+)
-}
mkPolyHomoFunc2 :: forall c. (forall t. c t => t -> t -> t) -> Poly ((IsHomoFunc 2) &&& ((Arg 0) `IxConstrainBy` c))
mkPolyHomoFunc2 = Poly

{-|
'mkPolyArgFunc2 represents a function from @t -> t -> r@, with two constraints, one for the arguments, one for the result.

For example, this is how to write a polymorphic version of "eq":

> mkPolyHomoArgFunc2 @Eq @(Equal Bool) (==)
-}
mkPolyHomoArgFunc2 :: forall c1 c2. (forall t1 t2. (c1 t1, c2 t2) => t1 -> t1 -> t2) -> Poly ((IsHomoArgFunc 2) &&& ((Arg 0) `IxConstrainBy` c1) &&& ((Result 2) `IxConstrainBy` c2))
mkPolyHomoArgFunc2 = Poly


class (IsFunc 1 f, c1 (IndexT 0 f), c2 (ResultT 1 f)) => PolyFunc1Constraints c1 c2 f
instance (IsFunc 1 f, c1 (IndexT 0 f), c2 (ResultT 1 f)) => PolyFunc1Constraints c1 c2 f

class (IsHomoFunc 1 f, c (IndexT 0 f)) => PolyHomoFunc1Constraints c f
instance (IsHomoFunc 1 f, c (IndexT 0 f)) => PolyHomoFunc1Constraints c f

{- $convinienceConstraintDocs
Below are some convience constraints that make it easier to write polymorphic functions.
-}

{-|
Handy type class for expressing an "is equal to" constraint, because as a class it can be partially applied.

For example, whilst @Num@ is a constraint function from @(* -> Constraint)@ such that @(Num t)@ succeeds only if
@t@ is a @Num@, @Equal Int@ is a constraint function such that @(Equal Int) t@ succeeds only if @t@ is an @Int@.

For example:

> mkPolyFunc1 @Integral @(Equal Integer) toInteger

Is a polymorphic 'toInteger' function.
-}
class (a ~ b) => Equal a b
instance (a ~ b) => Equal a b

{-|
The empty constraint:

> Empty a

always succeeds.
-}
class Empty a
instance Empty a


