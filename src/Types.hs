module Types
  ( Color(..)
  , Color3(..)
  , Color4(..)
  , GLdouble
  , GLfloat
  , GLubyte
  , Vector3(..)
  , Vertex3(..)
  , P3(..)
  , V3
  , translate
  )
where

import Graphics.Rendering.OpenGL (Color3(..), Color4(..), GLdouble, GLfloat, GLubyte, Vector3(..), Vertex3(..))
import GLInstances ()

type V3 = Vector3

data P3 = P3 !Int !Int !Int deriving (Eq, Ord, Show)

data Color = Red | Green | Blue | Yellow | White | Black
   deriving (Eq, Ord, Show)


translate :: V3 Int -> P3 -> P3
translate (Vector3 a b c) (P3 x y z) = P3 (x + a) (y + b) (z + c)
