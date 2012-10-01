module Numeric.LinearAlgebra.Matrix.Class where

-- import Numeric.LinearAlgebra.Vector

class Functor m => Matrix m where
  mDim     :: m a -> Int
  mElement :: m a -> Int -> Int -> a
  mIndexOf :: (Ord a) => (a -> a -> Bool) -> m a -> (Int, Int)
  mZip     :: (a -> b -> c) -> m a -> m b -> m c
  -- | mFold is foldl1'
  mFold    :: (a -> a -> a) -> m a -> a
  det      :: Num a => m a -> a

{-# INLINE mApply #-}
mApply :: Functor f => f (a -> b) -> a -> f b
mApply f m = fmap ($ m) f

(.+.) :: (Num k, Matrix m) => m k -> m k -> m k
(.+.) = mZip (+)
(.-.) :: (Num k, Matrix m) => m k -> m k -> m k
(.-.) = mZip (-)

{-
(.*.) :: (k ~ Element m, Num k, Matrix m) => m -> m -> m
m .*. n = mIdxMap m $ \i j -> sum [ mElement m i k * mElement n k j | k <- [ 0 .. 3 ] ]
-}
{-
(.*>) :: (k ~ Element m, k ~ Scalar v, Num k, Matrix m, Vector v)
      => m -> v -> v
m .*> v | vDim v == mDim m = flip vIdxMap v $ \k -> sum [ mElement m i k * vElement v k | i <- [ 0 .. mDim m ] ]
        | otherwise        = error "Dimensions do not match"
-}
-- (*.) :: (k ~ Element m, Num k, Matrix m) => m -> k -> m
-- m *. k = mMap (k*) m

{-
transpose :: Matrix m => m -> m
transpose m = flip mIdxMap m $ \i j -> mElement j i m
-}
