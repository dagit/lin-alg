module Numeric.LinearAlgebra.Matrix.Mat33 where

import Numeric.LinearAlgebra.Matrix.Class

import Data.List ( foldl1' )

data Mat33 a = Mat33
  { m00 :: !a, m01 :: !a, m02 :: !a
  , m10 :: !a, m11 :: !a, m12 :: !a
  , m20 :: !a, m21 :: !a, m22 :: !a
  }
  deriving (Read, Show, Eq, Ord)

instance Matrix Mat33 where
  {-# INLINE mDim #-}
  mDim _ = 3
  {-# INLINE mElement #-}
  mElement m 0 0 = m00 m
  mElement m 0 1 = m01 m
  mElement m 0 2 = m02 m
  mElement m 1 0 = m10 m
  mElement m 1 1 = m11 m
  mElement m 1 2 = m12 m
  mElement m 2 0 = m20 m
  mElement m 2 1 = m21 m
  mElement m 2 2 = m22 m
  mElement _ i j = error ("Index " ++ show i ++ ", " ++ show j ++ ": out of range, must be 0,0 to 2,2")
  {-# INLINE mZip #-}
  mZip f m n = Mat33 (f (m00 m) (m00 n)) (f (m01 m) (m01 n)) (f (m02 m) (m02 n))
                     (f (m10 m) (m10 n)) (f (m11 m) (m11 n)) (f (m12 m) (m12 n))
                     (f (m20 m) (m20 n)) (f (m21 m) (m21 n)) (f (m22 m) (m22 n))
  {-# INLINE mFold #-}
  mFold f m = foldl1' f [ mElement m i j | i <- [ 0 .. 2 ], j <- [ 0 .. 2 ] ]
  {-# INLINE mIndexOf #-}
  mIndexOf p m = fst (foldl1' p' [ ((i,j), mElement m i j) | j <- [ 2, 1 .. 0 ], i <- [ 2, 1 .. 0 ] ])
    where
    p' x@(_, a) y@(_, a') | a `p` a'  = x
                          | otherwise = y
  {-# INLINE det #-}
  det (Mat33 a b c d e f g h i) = det33 a b c d e f g h i

instance Functor Mat33 where
  {-# INLINE fmap #-}
  fmap f m = Mat33 (f (m00 m)) (f (m01 m)) (f (m02 m))
                   (f (m10 m)) (f (m11 m)) (f (m12 m))
                   (f (m20 m)) (f (m21 m)) (f (m22 m))

{-# SPECIALIZE det33 :: Double -> Double -> Double
                     -> Double -> Double -> Double
                     -> Double -> Double -> Double
                     -> Double #-}
{-# SPECIALIZE det33 :: Float -> Float -> Float
                     -> Float -> Float -> Float
                     -> Float -> Float -> Float
                     -> Float #-}
det33 :: Num a
      => a -> a -> a
      -> a -> a -> a
      -> a -> a -> a
      -> a
det33 a b c d e f g h i = a*e*i + d*h*c + g*b*f - g*e*c - d*b*i - a*h*f

{- SPECIALIZE inv33 :: Mat33 Double -> Mat33 Double -}
{- SPECIALIZE inv33 :: Mat33 Float  -> Mat33 Float -}
{-
inv33 :: Fractional a => Mat33 a -> Mat33 a
inv33 m = m'
  where
  d  = det m
  m' = Mat33 (m11 * m22 - m21 * m21)
  m' = Mat44 (det3 (m11 m) (m12 m) (m13 m)
                   (m21 m) (m22 m) (m23 m)
                   (m31 m) (m32 m) (m33 m) / d)
           (-(det3 (m01 m) (m02 m) (m03 m)
                   (m21 m) (m22 m) (m23 m)
                   (m31 m) (m32 m) (m33 m) / d))
             (det3 (m01 m) (m02 m) (m03 m)
                   (m11 m) (m12 m) (m13 m)
                   (m31 m) (m32 m) (m33 m) / d)
           (-(det3 (m01 m) (m02 m) (m03 m)
                   (m11 m) (m12 m) (m13 m)
                   (m21 m) (m22 m) (m23 m) / d))
             
           (-(det3 (m10 m) (m12 m) (m13 m)
                   (m20 m) (m22 m) (m23 m)
                   (m30 m) (m32 m) (m33 m) / d))
             (det3 (m00 m) (m02 m) (m03 m)
                   (m20 m) (m22 m) (m23 m)
                   (m30 m) (m32 m) (m33 m) / d)
           (-(det3 (m00 m) (m02 m) (m03 m)
                   (m10 m) (m12 m) (m13 m)
                   (m30 m) (m32 m) (m33 m) / d))
             (det3 (m00 m) (m02 m) (m03 m)
                   (m10 m) (m12 m) (m13 m)
                   (m20 m) (m22 m) (m23 m) / d)

             (det3 (m10 m) (m11 m) (m13 m)
                   (m20 m) (m21 m) (m23 m)
                   (m30 m) (m31 m) (m33 m) / d)
           (-(det3 (m00 m) (m01 m) (m03 m)
                   (m20 m) (m21 m) (m23 m)
                   (m30 m) (m31 m) (m33 m) / d))
             (det3 (m00 m) (m01 m) (m03 m)
                   (m10 m) (m11 m) (m13 m)
                   (m30 m) (m31 m) (m33 m) / d)
           (-(det3 (m00 m) (m01 m) (m03 m)
                   (m10 m) (m11 m) (m13 m)
                   (m20 m) (m21 m) (m23 m) / d))

           (-(det3 (m10 m) (m11 m) (m12 m)
                   (m20 m) (m21 m) (m22 m)
                   (m30 m) (m31 m) (m32 m) / d))
             (det3 (m00 m) (m01 m) (m02 m)
                   (m20 m) (m21 m) (m22 m)
                   (m30 m) (m31 m) (m32 m) / d)
           (-(det3 (m00 m) (m01 m) (m02 m)
                   (m10 m) (m11 m) (m12 m)
                   (m30 m) (m31 m) (m32 m) / d))
             (det3 (m00 m) (m01 m) (m02 m)
                   (m10 m) (m11 m) (m12 m)
                   (m20 m) (m21 m) (m22 m) / d)
-}
