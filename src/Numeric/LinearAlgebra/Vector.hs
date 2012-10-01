module Numeric.LinearAlgebra.Vector where

data Vec4 a = Vec4 !a !a !a !a
  deriving (Read, Show, Eq, Ord)
data Vec3 a = Vec3 !a !a !a
  deriving (Read, Show, Eq, Ord)
data Vec2 a = Vec2 !a !a
  deriving (Read, Show, Eq, Ord)

class Functor v => Vector v where
  vDim     :: v a -> Int
  vElement :: v a -> Int -> a
  vIndexOf :: (Ord a) => (a -> a -> Bool) -> v a -> Int
  vZip     :: (a -> b -> c) -> v a -> v b -> v c
  -- | vFold is foldl1'
  vFold    :: (a -> a -> a) -> v a -> a

instance Vector Vec4 where
  {-# INLINE vDim #-}
  vDim _ = 4
  {-# INLINE vElement #-}
  vElement (Vec4 x _ _ _) 0 = x
  vElement (Vec4 _ y _ _) 1 = y
  vElement (Vec4 _ _ z _) 2 = z
  vElement (Vec4 _ _ _ w) 3 = w
  vElement _ i = error ("Index " ++ show i ++ ": out of range, must be 0 to 3")
  {-# INLINE vZip #-}
  vZip f (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
  {-# INLINE vFold #-}
  vFold f (Vec4 x y z w) = f (f (f x y) z) w
  {-# INLINE vIndexOf #-}
  vIndexOf p (Vec4 x y z w) | w `p` x && w `p` y && w `p` z = 3
                            | z `p` x && z `p` y && z `p` w = 2
                            | y `p` x && y `p` z && y `p` w = 1
                            | otherwise = 0

instance Functor Vec4 where
  {-# INLINE fmap #-}
  fmap f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)

instance Vector Vec3 where
  {-# INLINE vDim #-}
  vDim _ = 3
  {-# INLINE vElement #-}
  vElement (Vec3 x _ _) 0 = x
  vElement (Vec3 _ y _) 1 = y
  vElement (Vec3 _ _ z) 2 = z
  vElement _ i = error ("Index " ++ show i ++ ": out of range, must be 0 to 2")
  {-# INLINE vZip #-}
  vZip f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
  {-# INLINE vFold #-}
  vFold f (Vec3 x y z) = f (f x y) z
  {-# INLINE vIndexOf #-}
  vIndexOf p (Vec3 x y z) | z `p` x && z `p` y = 2
                          | y `p` x && y `p` z = 1
                          | otherwise = 0

instance Functor Vec3 where
  {-# INLINE fmap #-}
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Vector Vec2 where
  {-# INLINE vDim #-}
  vDim _ = 2
  {-# INLINE vElement #-}
  vElement (Vec2 x _) 0 = x
  vElement (Vec2 _ y) 1 = y
  vElement _ i = error ("Index " ++ show i ++ ": out of range, must be 0 or 1")
  {-# INLINE vZip #-}
  vZip f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (f x1 x2) (f y1 y2)
  {-# INLINE vFold #-}
  vFold f (Vec2 x y) = f x y
  {-# INLINE vIndexOf #-}
  vIndexOf p (Vec2 x y) | y `p` x   = 1
                        | otherwise = 0

instance Functor Vec2 where
  {-# INLINE fmap #-}
  fmap f (Vec2 x y) = Vec2 (f x) (f y)

{-# INLINE vNegate #-}
vNegate :: (Num a, Vector v) => v a -> v a
vNegate = fmap negate
{-# INLINE (<+>) #-}
(<+>) :: (Num a, Vector v) => v a -> v a -> v a
(<+>) = vZip (+)
{-# INLINE (<*>) #-}
(<*>) :: (Num a, Vector v) => v a -> v a -> v a
(<*>) = vZip (*)
{-# INLINE (<->) #-}
(<->) :: (Num a, Vector v) => v a -> v a -> v a
(<->) = vZip (-)
{-# INLINE (</>) #-}
(</>) :: (Fractional a, Vector v) => v a -> v a -> v a
(</>) = vZip (/)
{-# INLINE (<.>) #-}
(<.>) :: (Num a, Vector v) => v a -> v a -> a -- dot product
v1 <.> v2 = vFold (+) (v1 <*> v2)
{-# INLINE (*>) #-}
(*>) :: (Num a, Vector v) => a -> v a -> v a
k *> v = fmap (k*) v
{-# INLINE (</) #-}
(</) :: (Fractional a, Vector v) => v a -> a -> v a
v </ k = fmap (/k) v
{-# INLINE len #-}
len :: (Floating a, Vector v) => v a -> a
len v = sqrt $ lenSquared v
{-# INLINE lenSquared #-}
lenSquared :: (Num a, Vector v) => v a -> a
lenSquared v = v <.> v

maxVec :: (Ord k, Vector v) => v k -> v k -> v k
maxVec = vZip max
minVec :: (Ord k, Vector v) => v k -> v k -> v k
minVec = vZip min
minComponent :: (Ord k, Vector v) => v k -> k
minComponent = vFold min
maxComponent :: (Ord k, Vector v) => v k -> k
maxComponent = vFold max
minAbsComponent :: (Num k, Ord k, Vector v) => v k -> k
minAbsComponent = vFold (\x y -> min (abs x) (abs y))
maxAbsComponent :: (Num k, Ord k, Vector v) => v k -> k
maxAbsComponent = vFold (\x y -> max (abs x) (abs y))
vIndexOfMinComponent :: (Ord k, Vector v) => v k -> Int
vIndexOfMinComponent = vIndexOf (<)
vIndexOfMaxComponent :: (Ord k, Vector v) => v k -> Int
vIndexOfMaxComponent = vIndexOf (>)
vIndexOfMinAbsComponent :: (Ord k, Num k, Vector v)
                        => v k -> Int
vIndexOfMinAbsComponent = vIndexOf (\x y -> abs x < abs y)
vIndexOfMaxAbsComponent :: (Ord k, Num k, Vector v)
                        => v k -> Int
vIndexOfMaxAbsComponent = vIndexOf (\x y -> abs x > abs y)

-- |Cross product
{-# INLINE (<%>) #-}
(<%>) :: Floating a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) <%> (Vec3 x2 y2 z2) = Vec3 (y1*z2 - z1*y2)
                                           (z1*x2 - x1*z2)
                                           (x1*y2 - y1*x2)

{-# INLINE unitVector #-}
unitVector :: (Floating a, Vector v) => v a -> v a
unitVector v = v </ len v

tripleProduct :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> a
tripleProduct v1 v2 v3 = (v1 <%> v2) <.> v3
