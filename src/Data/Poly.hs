{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
This package allows one to wrap data in a type: 'Poly', which explicitly carries around that's type's polymorphism.

This idea is motivated by this problem:

How does one write a function @g@ such that

>>> g f (x,y) = (f x, f y)

that works for all @a@ and @b@ where @f a@ and @f b@ are valid.

Lets try some approaches in ghci:

>>> let g f (a,b) = (f a, f b)
>>> :t
g :: (t1 -> t) -> (t1, t1) -> (t, t)

No good. As untyped function arguments are by default monomorphic, we've forced the pair to have two elements
the same type.

We could try this:

>>> let g (f :: (forall a b. a -> b)) (a,b) = (f a, f b)
>>> :t g
g :: (forall a2 b. a2 -> b) -> (a1, a) -> (t1, t)

but the only function with type @(forall a b. a -> b)@ is @undefined@, so that's pretty useless.

Perhaps we could do this:

>>> let g (f :: (forall a. Num a => a -> a)) (a,b) = (f a, f b)
>>> :t g
g :: (Num t1, Num t) =>
     (forall a. Num a => a -> a) -> (t1, t) -> (t1, t)

This is nice, then we can do something like:

>>> let h = g (+2) (1::Int, 2.5::Float)
>>> h
(3,4.5)
>>> :t h
h :: (Int, Float)

However, this only works for Numeric functions now.

So what we're going to do is connect the function's constraints with the function itself,
so we get a definition of @g@ like this:

> g :: (c (a -> a'), c (b -> b')) => Poly c -> (a, b) -> (a' -> b')

And indeed you can see polymorphic map function that works on heterogeneous tuples in 'Data.Poly.Functor'.

The 'Poly' type is quite generic, and indeed "Data.Poly.Function"
has some helper functions for constructing polymorphic functions directly.
-}
module Data.Poly (
  Poly(Poly, getPoly),
  GetPolyConstraint,
  IsPoly
  )
where

import GHC.Exts (Constraint)
{-|
'Poly' has the following data definition:

> data Poly (c :: * -> Constraint) where
>   Poly :: { getPoly :: (forall a. c a => a) } -> Poly c

Haddock has trouble parsing it, presumably because it's confused by @(c :: * -> Constraint)@.

Here's a first example, which is a polymorphic version of 'toInteger':

> polyToInteger = Poly @((IsFunc 1) &&& ((Arg 0) `IxConstrainBy` Integral) &&& ((Result 1) `IxIs` Integer)) toInteger

So lets look from left to right for what constraints we're passing to 'polyToInteger':

> (IsFunc 1)

'Control.IndexT.Function.IsFunc' constrains a type to be a function, in this case of one variable

> ((Arg 0) `IxConstrainBy` Integral)

'Control.ConstraintManip.Arg' @0@ specifies the first argument (this is zero based)
'Control.ConstraintManip.IxConstrainBy' constrains the argument given to the constraint given,
in this case 'Integral'

> ((Result 1) `IxIs` Integer)

So the 'Control.ConstraintManip.Result' (of the one argument function) is 'Integer'.

So then we can do:

> getPoly polyToInteger (10 :: Int) -- (10 :: Integer)

Our second example is probably simpler:

> triple = Poly @((IsHomoFunc 1) &&& ((Arg 0) `IxConstrainBy` Num)) (*3)

'Control.IndexT.Function.IsHomoFunc' is like 'Control.IndexT.Function.IsFunc' but ensures the two arguments are the same.

'Control.ConstraintManip.IxConstrainBy' we've already seen. Note that here:

> (Arg 0) `IxConstrainBy` Num

and

> (Result 1) `IxConstrainBy` Num

have the same effect because the first argument and the result are already constrained to have the same type from
'Control.IndexT.Function.IsHomoFunc'.

Two more examples, with two arguments, are:

> add = Poly @((IsHomoFunc 2) &&& ((Arg 0) `IxConstrainBy` Num)) (+)

and

> eq = Poly @((IsHomoArgFunc 2) &&& ((Arg 0) `IxConstrainBy` Eq) &&& ((Result 2) `IxIs` Bool)) (==)

'Control.IndexT.Function.IsHomoArgFunc', unlike 'Control.IndexT.Function.IsHomoFunc', just specifies that the arguments are
identical, the result may be different.

At this point it's probably worth looking at "Data.Poly.Function", which has a range of convience functions for making the
above definitions easier.

If you've now looked at "Data.Poly.Function", you've seen two ways to define the constraints to pass to 'Poly':

1) Use the convienience functions in "Data.Poly.Function"
2) Combine constraints of one variable with '(Control.ConstraintManip.&&&)' as detailed above.

But sometimes these above two methods aren't flexible enough to generate the polymorphic constraint required.

Consider 'Data.Foldable.foldl''

> foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

with something this complicated, its sometimes best to define the constraint directly ourselves.
So here it is:

> type FoldConstraint t = (
>   IsFunc 3 t, -- A fold is a function of three args
>   IndexT 1 t ~ ResultT 3 t, -- The second (i.e. arg 1) is equal to the result
>   IsFunc 2 (IndexT 0 t), -- the first argument (i.e. the fold function) is a function of two args
>   (IndexT 0 (IndexT 0 t)) ~ (ResultT 2 (IndexT 0 t)), -- the first argument of the function which is the first argument is the same as it's third
>   IndexT 1 t ~ (IndexT 0 (IndexT 0 t)), -- also, the first argument of the function which is the first argument is the same as the second argument of the function
>   IsData 1 (IndexT 2 t), -- the third argument is a data type with one variable
>   Foldable (GetConstructor1 (IndexT 2 t)), -- the constructor of that third argument is Foldable
>   IndexC 1 0 (IndexT 2 t) ~ IndexT 1 (IndexT 0 t) -- the parameter to the constructor of Foldable is the same as the second argument of the fold function
>   )

You'll want to look at the package "indextype" to get some details on these functions.

But if you go through the above slowly, you'll see that this constraint completely describes the sort of functions that
have the same signature as 'Data.Foldable.foldl''.

So then we can do this:

> class (FoldConstraint t) => FoldConstraintC t
> instance (FoldConstraint t) => FoldConstraintC t
>
> pfoldl' = Poly @FoldConstraintC foldl'
> polyFold (Poly foldFunc) =
>   (foldFunc (+) 0 [1,2,3], foldFunc (+) 0 [1.5,2.5,3.5], foldFunc (++) "" ["Hello", ", ", "World"])

And we can then do:

>>> (polyFold pfoldl') :: (Int, Float, String)
(6,7.5,"Hello, World")

Note that this wrapping approach preserves the polymorphism until inside the function.

At this point, you may ask, why not just define a new datatype with a polymorphic parameter each time you want to do this?

Well, firstly, you'd have to define a new datatype each time you want to pass a different type of function polymorphically,
which is a bit of boilerplate, although it's arguably less than this.

But more importantly, having a \"constraint\" on the type, instead of the actual type, allows as to use that constraint to
build more complex constraints.

A good example of that is 'Data.Poly.Functor.hmap'.

For complex functions, there can be a lot to write these constraints, but constraints are composable, so you can split
out common parts.

However, I have a feeling there is a mechanical way to generate these constraints using Template Haskell.
This will be my next addition to the library.
-}
data Poly (c :: * -> Constraint) where
  Poly :: { getPoly :: (forall a. c a => a) } -> Poly c

{-|
Gets the type of the constraint in a 'Poly'
-}
type family GetPolyConstraint a :: * -> Constraint where
  GetPolyConstraint (Poly c) = c

type family IsPolyT a :: Constraint where
  IsPolyT a = a ~ Poly (GetPolyConstraint a)

{-
Constraint that asserts @t@ is a @Poly u@ for some @u@.
-}
class (IsPolyT a) => IsPoly a
instance (IsPolyT a) => IsPoly a

