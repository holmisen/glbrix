{-# LANGUAGE DeriveFunctor #-}

module V3
  ( Vector3(..)
  , V3
  , zipV3
  )
where

import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..))

type V3 = Vector3

zipV3 f (Vector3 a b c) (Vector3 x y z) = Vector3 (f a x) (f b y) (f c z)

-- instance Num a => Num (V3 a) where
--    (+) = zipV3 (+)
--    (-) = zipV3 (-)
--    (*) = zipV3 (*)
--    abs = fmap abs
--    signum = fmap signum
--    fromInteger = V3 0 0 . fromInteger
