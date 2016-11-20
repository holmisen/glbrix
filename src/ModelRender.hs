module ModelRender where

import Data.Foldable (traverse_)
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Primitive
import Types

--------------------------------------------------------------------------------

renderModel :: [PlacedPart] -> IO ()
renderModel = traverse_ (renderPart renderColor)

type Renderer a = (Color -> IO ()) -> a -> IO ()

renderWithUniqColors :: Renderer a -> [a] -> IO ()
renderWithUniqColors render =
   traverse_ (\(color, part) -> render (const $ GL.color color) part) . zip [zero ..]
   where
      zero = toEnum 0 :: Color3 GLubyte


renderPart :: Renderer PlacedPart
renderPart cf = traverse_ (renderPlaced cf)

renderPlaced :: Renderer (Placed Prim)
renderPlaced renderColor (Placed p c a) =
   GL.preservingMatrix $ do
      renderPlacement p
      traverse_ renderColor c
      Primitive.render a

renderPlacement (Placement p r) = do
   renderPosition p
   renderRotation r

renderPosition (P3 x y z) =
   GL.translate (vector3f (2 * fromIntegral x) (2 * fromIntegral y) (fromIntegral z))

renderRotation (Rotation r) = GL.rotate (90 * fromIntegral r) (vector3f 0 0 1)

renderColor :: Color -> IO ()
renderColor = GL.color . go
   where
      c r g b = Color3 r g b :: Color3 GLfloat
      go Red    = c 1 0 0
      go Green  = c 0 1 0
      go Blue   = c 0 0 1
      go Yellow = c 1 1 0
      go White  = c 0.8 0.8 0.8
      go Black  = c 0.2 0.2 0.2

--------------------------------------------------------------------------------

vector3f x y z = Vector3 x y z :: Vector3 GLdouble
vertex3f x y z = Vertex3 x y z :: Vertex3 GLdouble

renderAxis s =
  GL.renderPrimitive GL.Lines $ do
    GL.color (Color3 1 0 0 :: Color3 GLfloat)
    GL.vertex $ vertex3f 0 0 0
    GL.vertex $ vertex3f s 0 0

    GL.color (Color3 0 1 0 :: Color3 GLfloat)
    GL.vertex $ vertex3f 0 0 0
    GL.vertex $ vertex3f 0 s 0

    GL.color (Color3 0 0 1 :: Color3 GLfloat)
    GL.vertex $ vertex3f 0 0 0
    GL.vertex $ vertex3f 0 0 s
