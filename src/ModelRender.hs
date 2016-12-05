module ModelRender where

import Data.Foldable (for_, traverse_)
import Data.StateVar
import Graphics.Rendering.OpenGL.GL.VertexArrays (Capability(..))
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Primitive
import Types
import qualified GLColor

--------------------------------------------------------------------------------

renderModel :: [PlacedPart] -> IO ()
renderModel = traverse_ (renderPart renderPrimBrick)

renderModelWireframe :: Foldable t => t PlacedPart -> IO ()
renderModelWireframe = traverse_ (renderPart renderPrimWireframe)

renderWithUniqColors :: Renderer a -> [a] -> IO ()
renderWithUniqColors renderer xs =
   for_ (zip [zero ..] xs) $ \(color, part) ->
      renderer (const $ renderPrimShape color) part
   where
      zero = toEnum 0 :: Color3 GLubyte

--------------------------------------------------------------------------------

type Renderer a = PrimRenderer (Color3 GLfloat) -> a -> IO ()

renderPart :: Renderer PlacedPart
renderPart renderer = traverse_ (renderPlaced renderer)

renderPlaced :: Renderer (Placed Prim)
renderPlaced renderPrim (Placed p c a) =
   GL.preservingMatrix $ do
      renderPlacement p

      -- This translation is a hack to make world coordinates refer to
      -- the center of the position rather than its lower left
      -- corner. Otherwise, rotations would never include the same
      -- point.
      GL.translate (vector3f (-1) (-1) 0)
      renderPrim (toGLColor c) a

--------------------------------------------------------------------------------

type PrimRenderer color = color -> Prim -> IO ()

renderPrimBrick :: GL.Color c => PrimRenderer c
renderPrimBrick color a = do
   renderPrimWireframe GLColor.wireframe a
   renderPrimShape color a

renderPrimShape :: GL.Color c => PrimRenderer c
renderPrimShape color a = do
   GL.polygonMode $= (GL.Fill, GL.Fill)
   GL.polygonOffset $= (1,1)
   GL.polygonOffsetFill $= Enabled
   GL.color color
   Primitive.render a
   GL.polygonOffsetFill $= Disabled

renderPrimWireframe :: GL.Color c => PrimRenderer c
renderPrimWireframe color a = do
   GL.polygonMode $= (GL.Line, GL.Line)
   GL.color color
   Primitive.render a

--------------------------------------------------------------------------------

renderPlacement :: Placement -> IO ()
renderPlacement (Placement p r) = do
   renderPosition p
   renderRotation r

renderPosition :: P3 -> IO ()
renderPosition (P3 x y z) =
   GL.translate (vector3f (2 * fromIntegral x) (2 * fromIntegral y) (fromIntegral z))

renderRotation :: Rotation -> IO ()
renderRotation (Rotation r) = GL.rotate (90 * fromIntegral r) (vector3f 0 0 1)

--------------------------------------------------------------------------------

toGLColor :: Color -> Color3 GLfloat
toGLColor Black     = GLColor.black
toGLColor Blue      = GLColor.blue
toGLColor Brown     = GLColor.brown
toGLColor DarkBlue  = GLColor.darkBlue
toGLColor DarkGray  = GLColor.darkGray
toGLColor DarkGreen = GLColor.darkGreen
toGLColor Gray      = GLColor.gray
toGLColor Green     = GLColor.green
toGLColor LightBlue = GLColor.lightBlue
toGLColor Red       = GLColor.red
toGLColor Tan       = GLColor.tan
toGLColor White     = GLColor.white
toGLColor Yellow    = GLColor.yellow

--------------------------------------------------------------------------------

vector3f x y z = Vector3 x y z :: Vector3 GLdouble
vertex3f x y z = Vertex3 x y z :: Vertex3 GLdouble

renderAxis :: GLdouble -> IO ()
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
