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
  , lposX, lposY, lposZ
  , V3
  , translate
  , vectorBetween
  )
where

import Graphics.Rendering.OpenGL (Color3(..), Color4(..), GLdouble, GLfloat, GLubyte, Vector3(..), Vertex3(..))
import GLInstances ()
import Lens.Micro

type V3 = Vector3

data P3 = P3 !Int !Int !Int deriving (Eq, Ord, Show)

lposX :: Lens' P3 Int
lposX = lens (\(P3 x _ _) -> x) (\(P3 _ y z) x -> P3 x y z)

lposY :: Lens' P3 Int
lposY = lens (\(P3 _ y _) -> y) (\(P3 x _ z) y -> P3 x y z)

lposZ :: Lens' P3 Int
lposZ = lens (\(P3 _ _ z) -> z) (\(P3 x y _) z -> P3 x y z)


data Color
    = Black
    | Blue
    | Brown
    | DarkBlue
    | DarkGray
    | DarkGreen
    | Gray
    | Green
    | LightBlue
    | Red
    | Tan
    | White
    | Yellow
   deriving (Eq, Ord, Show)


translate :: V3 Int -> P3 -> P3
translate (Vector3 a b c) (P3 x y z) = P3 (x + a) (y + b) (z + c)


vectorBetween :: P3 -> P3 -> V3 Int
vectorBetween (P3 a b c) (P3 x y z) = Vector3 (x-a) (y-b) (z-c)
