{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GLInstances where

import Graphics.Rendering.OpenGL


instance (Bounded a, Integral a) => Enum (Color3 a) where
   fromEnum (Color3 a b c) = fi a * m^2 + fi b * m + fi c where
      m = 1 + fi (maxBound :: a)
      fi = fromIntegral

   toEnum i = Color3 (fi a) (fi b) (fi c) where
      fi x = fromIntegral x
      m = 1 + fi (maxBound :: a)
      (a,i') = quotRem i  (m^2)
      (b,c)  = quotRem i' m
