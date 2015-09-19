-----------------------------------------------------------------------------
--
-- Module      :  Unions
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Michael J. Palmer
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds, ConstraintKinds, KindSignatures, TypeOperators, MultiParamTypeClasses,
    FlexibleInstances, ExistentialQuantification, ScopedTypeVariables, UndecidableInstances,
    OverlappingInstances, Rank2Types, FlexibleContexts #-}
module Unions (
  In(downcast), Union, upcast, dispatch, Intersection, (<&>), emptyIntersection, MakeIntersection(..),
  Poly1(..), Poly2(..)
) where

import GHC.Prim (Constraint)
import Data.Vector
import Unsafe.Coerce

-- The Index type is not exported.
-- Its intended use is for the integer value to be entirely determined
-- by the (type, type-list) parameters. The 'In' typeclass is designed
-- to construct Index values that work this way.

newtype Index a (u :: [*]) = Index {unIndex :: Int}

class In a (u :: [*]) where
  index :: Index a u
  downcast :: Union u -> Maybe a
  downcast (Union tag v) =
    if (unIndex tag) == unIndex (index :: Index a u)  -- I need to write this function in the type class definition
    then Just (unsafeCoerce v)                        -- in order to use scoped type variables for this
    else Nothing                                      -- type annotation

instance In a (a ': t) where
  index = Index 0
instance In a t => In a (b ': t) where
  index = Index (1 + unIndex (index :: Index a t))

-- This type is an above-the-board representation of a tagged union.
-- The member types of the union are defined by the type parameter u, which is a list of types.
data Union (u :: [*]) = forall a. (a `In` u) => Union !(Index a u) a

upcast :: (a `In` u) => a -> Union u
upcast a = Union index a

data ExtFunction b = forall a. ExtFunction (a -> b)

-- For every type in the type list, an intersection contains a function that can take
-- that type as its input. All of the functions in the intersection produce outputs of
-- the same type. Internally, the functions are arranged in a vector in the same order as
-- the list of input types passed as a type parameter.
data Intersection (u :: [*]) b = Intersection (Vector (ExtFunction b))

emptyIntersection = Intersection (fromList [])

-- Adds a function to the front of an intersection, updating the type list accordingly
infixr 9 <&>
(<&>) :: (a -> b) -> Intersection t b -> Intersection (a ': t) b
(<&>) f (Intersection vect) = Intersection (cons (ExtFunction f) vect)

-- Given an intersection and a union, locates the appropriate function in the intersection
-- to handle the union's data and calls it.
dispatch :: Intersection u b -> Union u -> b
dispatch (Intersection vect) (Union tag v) = case vect ! unIndex tag of
  ExtFunction f -> ((unsafeCoerce f) :: a -> b) ((unsafeCoerce v) :: a)
-- I'm using unsafeCoerce here, so I need to be sure that I know whar I'm doing.
-- The assumption here is that the union's integer tag is the same as the index of
-- the function in the intersection's vector whose input type matches that of the union's data.
-- This invariant needs to be enforced by not exposing the internals of Union or Intersection.

-- These types wrap some rank-2 function types in order to use them in typeclass instances.
data Poly1 c r = Poly1 (forall a. c a => a -> r)
data Poly2 c r = Poly2 (forall a b. c a b => a -> b -> r)

class MakeIntersection a isec where
  mkIntersection :: a -> isec

instance MakeIntersection a (Intersection '[] b) where
  mkIntersection a = emptyIntersection

instance (c a, MakeIntersection (Poly1 c b) (Intersection t b)) =>
    MakeIntersection (Poly1 c b) (Intersection (a ': t) b) where
  mkIntersection (p@(Poly1 f)) = f <&> mkIntersection p

ap1 :: Poly2 c b -> a -> Poly1 (c a) b
ap1 (Poly2 f) a = Poly1 (f a)

instance (MakeIntersection (Poly1 (c a) b) (Intersection u2 b),
          MakeIntersection (Poly2 c b) (Intersection t (Intersection u2 b))) =>
    MakeIntersection (Poly2 c b) (Intersection (a ': t) (Intersection u2 b)) where
  mkIntersection p = (mkIntersection . ap1 p) <&> mkIntersection p
