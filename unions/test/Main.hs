
{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, FlexibleInstances, OverlappingInstances,
    ConstraintKinds, MultiParamTypeClasses, NoMonomorphismRestriction#-}
module Main (
    main
) where



import Unions
import Control.Monad (forM)

type TestUnion = Union '[String, Int, Bool]

showIntersection = mkIntersection ((Poly1 show) :: Poly1 Show String) :: Intersection '[String, Int, Bool] String

uval :: (String `In` u, Int `In` u, Bool `In` u) => Int -> Union u
uval i = case i of
  0 -> upcast False
  1 -> upcast "One"
  other -> upcast other

data Foo = Foo
data Bar = Bar

class Test2Param a b where
  test2Param :: a -> b -> Int

instance Test2Param Foo Foo where
  test2Param Foo Foo = 11

instance Test2Param Foo Bar where
  test2Param Foo Bar = 10

instance Test2Param Bar Foo where
  test2Param Bar Foo = 1

instance Test2Param Bar Bar where
  test2Param Bar Bar = 0

instance Test2Param (Union '[Foo, Bar]) (Union '[Foo, Bar]) where
  test2Param = let
      test2ParamIntersection :: Intersection '[Foo, Bar] (Intersection '[Foo, Bar] Int)
      test2ParamIntersection = mkIntersection ((Poly2 test2Param) :: Poly2 Test2Param Int)
    in \a b -> dispatch (dispatch test2ParamIntersection a) b

foobar :: Int -> Union '[Foo, Bar]
foobar i = if i `mod` 2 == 0 then upcast Foo else upcast Bar


main = do
  forM [0, 1, 2] $ \i -> putStrLn (dispatch showIntersection (uval i))
  forM [0, 1] $ \i -> forM [0, 1] $ \j -> putStrLn (show (test2Param (foobar i) (foobar j)))
