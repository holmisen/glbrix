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
   | Block Int Int Int
   deriving (Eq, Ord, Show)

height :: Prim -> Int
height (Brick {})    = 3
height (Plate {})    = 1
height (Block _ _ h) = 3*h

render :: Prim -> IO ()
render (Block l w h) = renderBeam (2 * fromIntegral l) (2 * fromIntegral w) (fromIntegral (3*h))
render (Plate l w)   = renderBeam (2 * fromIntegral l) (2 * fromIntegral w) (fromIntegral 1)
render (Brick l w)   = renderBeam (2 * fromIntegral l) (2 * fromIntegral w) (fromIntegral 3)

-- | Render primitive without any details. For now defined as
-- 'render', but might change.
renderShape :: Prim -> IO ()
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
   GL.renderPrimitive GL.Quads $ do
      -- upper:
      GL.vertex $ vx3d 0 0 h
      GL.vertex $ vx3d w 0 h
      GL.vertex $ vx3d w l h
      GL.vertex $ vx3d 0 l h
      -- lower:
      GL.vertex $ vx3d 0 0 0
      GL.vertex $ vx3d 0 l 0
      GL.vertex $ vx3d w l 0
      GL.vertex $ vx3d w 0 0
      -- front:
      GL.vertex $ vx3d 0 0 0
      GL.vertex $ vx3d w 0 0
      GL.vertex $ vx3d w 0 h
      GL.vertex $ vx3d 0 0 h
      -- back:
      GL.vertex $ vx3d 0 l 0
      GL.vertex $ vx3d 0 l h
      GL.vertex $ vx3d w l h
      GL.vertex $ vx3d w l 0
      -- left:
      GL.vertex $ vx3d 0 0 0
      GL.vertex $ vx3d 0 0 h
      GL.vertex $ vx3d 0 l h
      GL.vertex $ vx3d 0 l 0
      -- right:
      GL.vertex $ vx3d w 0 0
      GL.vertex $ vx3d w l 0
      GL.vertex $ vx3d w l h
      GL.vertex $ vx3d w 0 h

--------------------------------------------------------------------------------

vx2d x y = GL.Vertex2 x y :: GL.Vertex2 GL.GLdouble

vx3d x y z = GL.Vertex3 x y z :: GL.Vertex3 GL.GLdouble

translated = GL.translate :: Vector3 Double -> IO ()

rotated :: Double -> GL.Vector3 Double ->  IO ()
rotated = GL.rotate
