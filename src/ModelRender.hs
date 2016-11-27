module ModelRender where

import Data.Foldable (traverse_)
import Data.StateVar
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.VertexArrays (Capability(..))

import Model
import Primitive
import Types
import qualified GLColor

--------------------------------------------------------------------------------

renderModel :: [PlacedPart] -> IO ()
renderModel parts = do
   -- Render part wireframe in constant color
   GL.polygonMode $= (GL.Line, GL.Line)
   traverse_ (renderPart $ const $ GL.color $ Color3 0.2 0.2 (0.2 :: GLfloat)) parts

   -- Render part polygons
   GL.polygonMode $= (GL.Fill, GL.Fill)
   GL.polygonOffset $= (1,1)
   GL.polygonOffsetFill $= Enabled
   traverse_ (renderPart renderColor) parts
   GL.polygonOffsetFill $= Disabled


renderModelWireframe :: Foldable t => t PlacedPart -> IO ()
renderModelWireframe parts = do
   -- Render part wireframe in the parts color
   GL.polygonMode $= (GL.Line, GL.Line)
   traverse_ (renderPart renderColor) parts


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
      go Black     = GLColor.black
      go Blue      = GLColor.blue
      go Green     = GLColor.green
      go LightBlue = GLColor.lightBlue
      go Red       = GLColor.red
      go Tan       = GLColor.tan
      go White     = GLColor.white
      go Yellow    = GLColor.yellow

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
