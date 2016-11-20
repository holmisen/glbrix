module Primitive
  ( Prim(..)
  , height
  , render
  , renderShape
  )
where

import Types

import Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

data Prim
   = Brick Int Int
   | Plate Int Int
   deriving (Eq, Ord, Show)

height (Brick {}) = 3
height (Plate {}) = 1

render (Brick l w) = renderBeam (2 * fromIntegral l) (2 * fromIntegral w) (fromIntegral 3)
render (Plate l w) = renderBeam (2 * fromIntegral l) (2 * fromIntegral w) (fromIntegral 1)

renderShape = render

renderRect :: Double -> Double -> IO ()
renderRect l w =
   GL.renderPrimitive GL.Quads $ do
      GL.vertex (vx2d 0 0)
      GL.vertex (vx2d w 0)
      GL.vertex (vx2d w l)
      GL.vertex (vx2d 0 l)

renderBeam :: Double -> Double -> Double -> IO ()
renderBeam l w h = do
   -- upper:
   GL.preservingMatrix $ do
      translated (Vector3 0 0 h)
      renderRect l w
   -- lower:
   GL.preservingMatrix $ do
      translated (Vector3 0 l 0)
      rotated 180 (Vector3 1 0 0)
      renderRect l w
   -- front:
   GL.preservingMatrix $ do
      rotated 90 (Vector3 1 0 0)
      renderRect h w
   -- back:
   GL.preservingMatrix $ do
      translated (Vector3 0 l h)
      rotated (-90) (Vector3 1 0 0)
      renderRect h w
   -- left:
   GL.preservingMatrix $ do
      rotated (-90) (Vector3 0 1 0)
      renderRect l h
   -- right:
   GL.preservingMatrix $ do
      translated (Vector3 w 0 h)
      rotated 90 (Vector3 0 1 0)
      renderRect l h


--------------------------------------------------------------------------------

vx2d x y = GL.Vertex2 x y :: GL.Vertex2 GL.GLdouble

vx3d x y z = GL.Vertex3 x y z :: GL.Vertex3 GL.GLdouble

translated = GL.translate :: Vector3 Double -> IO ()

rotated :: Double -> GL.Vector3 Double ->  IO ()
rotated = GL.rotate
