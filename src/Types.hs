module Types
   (
   -- * Colors
     Color(..)
   , Color3(..)
   , Color4(..)

   -- * Scalars
   , GLdouble
   , GLfloat
   , GLubyte

   -- * Other
   , Vector3(..)
   , Vertex3(..)

   -- * Positions
   , P3(..)
   , lposX, lposY, lposZ
   , origo

   -- * Vectors
   , V3
   , translate
   , rotateZ
   , vectorBetween
   , vectorAdd
   )
where

import Graphics.Rendering.OpenGL (Color3(..), Color4(..), GLdouble, GLfloat, GLubyte, Vector3(..), Vertex3(..))
import GLInstances ()
import Lens.Micro

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data P3 = P3 !Int !Int !Int deriving (Eq, Ord, Show)

origo :: P3
origo = P3 0 0 0

lposX :: Lens' P3 Int
lposX = lens (\(P3 x _ _) -> x) (\(P3 _ y z) x -> P3 x y z)

lposY :: Lens' P3 Int
lposY = lens (\(P3 _ y _) -> y) (\(P3 x _ z) y -> P3 x y z)

lposZ :: Lens' P3 Int
lposZ = lens (\(P3 _ _ z) -> z) (\(P3 x y _) z -> P3 x y z)

--------------------------------------------------------------------------------

type V3 = Vector3

translate :: V3 Int -> P3 -> P3
translate (Vector3 a b c) (P3 x y z) = P3 (x + a) (y + b) (z + c)


-- | Rotate full 90 degrees around Z. Later, use floating point P3 and
-- enable arbitrary rotations.
rotateZ :: Int -> P3 -> P3
rotateZ n = go (n `mod` 4)
   where
      go 0 p = p
      go 1 (P3 x y z) = P3 (negate y) x          z
      go 2 (P3 x y z) = P3 (negate x) (negate y) z
      go 3 (P3 x y z) = P3 y          (negate x) z
      -- x' = negate y
      -- y' = x


vectorBetween :: P3 -> P3 -> V3 Int
vectorBetween (P3 a b c) (P3 x y z) = Vector3 (x-a) (y-b) (z-c)


vectorAdd :: Num a => Vector3 a -> Vector3 a -> Vector3 a
vectorAdd (Vector3 a b c) (Vector3 x y z) = Vector3 (a+x) (b+y) (c+z)
