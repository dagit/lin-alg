module Numeric.LinearAlgebra.Matrix.Mat44 where

import Numeric.LinearAlgebra.Matrix.Class
import Numeric.LinearAlgebra.Matrix.Mat33 ( det33 )

import Data.List ( foldl1' )
import Data.NumInstances() -- Add function instances to Num

data Mat44 a = Mat44
  { m00 :: !a, m01 :: !a, m02 :: !a, m03 :: !a
  , m10 :: !a, m11 :: !a, m12 :: !a, m13 :: !a
  , m20 :: !a, m21 :: !a, m22 :: !a, m23 :: !a
  , m30 :: !a, m31 :: !a, m32 :: !a, m33 :: !a
  }
  deriving (Read, Show, Eq, Ord)

instance Matrix Mat44 where
  {-# INLINE mDim #-}
  mDim _ = 4
  {-# INLINE mElement #-}
  mElement m 0 0 = m00 m
  mElement m 0 1 = m01 m
  mElement m 0 2 = m02 m
  mElement m 0 3 = m03 m
  mElement m 1 0 = m10 m
  mElement m 1 1 = m11 m
  mElement m 1 2 = m12 m
  mElement m 1 3 = m13 m
  mElement m 2 0 = m20 m
  mElement m 2 1 = m21 m
  mElement m 2 2 = m22 m
  mElement m 2 3 = m23 m
  mElement m 3 0 = m30 m
  mElement m 3 1 = m31 m
  mElement m 3 2 = m32 m
  mElement m 3 3 = m33 m
  mElement _ i j = error ("Index " ++ show i ++ ", " ++ show j ++ ": out of range, must be 0,0 to 3,3")
  {-# INLINE mZip #-}
  mZip f m n = Mat44 (f (m00 m) (m00 n)) (f (m01 m) (m01 n)) (f (m02 m) (m02 n)) (f (m03 m) (m03 n))
                     (f (m10 m) (m10 n)) (f (m11 m) (m11 n)) (f (m12 m) (m12 n)) (f (m13 m) (m13 n))
                     (f (m20 m) (m20 n)) (f (m21 m) (m21 n)) (f (m22 m) (m22 n)) (f (m23 m) (m23 n))
                     (f (m30 m) (m30 n)) (f (m31 m) (m31 n)) (f (m32 m) (m32 n)) (f (m33 m) (m33 n))
  {-# INLINE mFold #-}
  mFold f m = foldl1' f [ mElement m i j | i <- [ 0 .. 3 ], j <- [ 0 .. 3 ] ]
  {-# INLINE mIndexOf #-}
  mIndexOf p m = fst (foldl1' p' [ ((i,j), mElement m i j) | j <- [ 3, 2 .. 0 ], i <- [ 3, 2 .. 0 ] ])
    where
    p' x@(_, a) y@(_, a') | a `p` a'  = x
                          | otherwise = y
  {-# INLINE det #-}
  det = det44

instance Functor Mat44 where
  {-# INLINE fmap #-}
  fmap f m = Mat44 (f (m00 m)) (f (m01 m)) (f (m02 m)) (f (m03 m))
                   (f (m10 m)) (f (m11 m)) (f (m12 m)) (f (m13 m))
                   (f (m20 m)) (f (m21 m)) (f (m22 m)) (f (m23 m))
                   (f (m30 m)) (f (m31 m)) (f (m32 m)) (f (m33 m))

{-# SPECIALIZE det44 :: Mat44 Double -> Double #-}
{-# SPECIALIZE det44 :: Mat44 Float  -> Float #-}
det44 :: Num a => Mat44 a -> a
det44 = m00 * det33 m11 m12 m13
                    m21 m22 m23
                    m31 m32 m33
      - m01 * det33 m10 m12 m13
                    m20 m22 m23
                    m30 m32 m33
      + m02 * det33 m10 m11 m13
                    m20 m21 m23
                    m30 m31 m33
      - m03 * det33 m10 m11 m12
                    m20 m21 m22
                    m30 m31 m32

{-# SPECIALIZE inv44 :: Mat44 Double -> Mat44 Double #-}
{-# SPECIALIZE inv44 :: Mat44 Float  -> Mat44 Float  #-}
inv44 :: Fractional a => Mat44 a -> Mat44 a
inv44 m = mApply m' m
  where
  d  = const $ det44 m
  m' = Mat44 (det33 m11 m12 m13
                    m21 m22 m23
                    m31 m32 m33 / d)
           (-(det33 m01 m02 m03
                    m21 m22 m23
                    m31 m32 m33 / d))
             (det33 m01 m02 m03
                    m11 m12 m13
                    m31 m32 m33 / d)
           (-(det33 m01 m02 m03
                    m11 m12 m13
                    m21 m22 m23 / d))
               
           (-(det33 m10 m12 m13
                    m20 m22 m23
                    m30 m32 m33 / d))
             (det33 m00 m02 m03
                    m20 m22 m23
                    m30 m32 m33 / d)
           (-(det33 m00 m02 m03
                    m10 m12 m13
                    m30 m32 m33 / d))
             (det33 m00 m02 m03
                    m10 m12 m13
                    m20 m22 m23 / d)

             (det33 m10 m11 m13
                    m20 m21 m23
                    m30 m31 m33 / d)
           (-(det33 m00 m01 m03
                    m20 m21 m23
                    m30 m31 m33 / d))
             (det33 m00 m01 m03
                    m10 m11 m13
                    m30 m31 m33 / d)
           (-(det33 m00 m01 m03
                    m10 m11 m13
                    m20 m21 m23 / d))

           (-(det33 m10 m11 m12
                    m20 m21 m22
                    m30 m31 m32 / d))
             (det33 m00 m01 m02
                    m20 m21 m22
                    m30 m31 m32 / d)
           (-(det33 m00 m01 m02
                    m10 m11 m12
                    m30 m31 m32 / d))
             (det33 m00 m01 m02
                    m10 m11 m12
                    m20 m21 m22 / d)
