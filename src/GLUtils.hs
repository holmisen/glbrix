module GLUtils where

import Types

import Data.StateVar (get)
import Foreign.Marshal.Alloc as Ptr
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Ptr           (Ptr)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT          as GLUT

--------------------------------------------------------------------------------

flipPosition :: Position -> IO Position
flipPosition (Position posX posY) = do
   (Size _ h) <- get GLUT.windowSize
   return $ Position posX (fromIntegral h - posY)


-- | Get window Z coordinate for given window position.
getWindowZ :: Position -> IO GLfloat
getWindowZ pos  = do
   GL.rowAlignment Unpack $= 1
   GL.readBuffer $= BackBuffers

   ptr <- mallocArray 1 :: IO (Ptr GLfloat)
   GL.readPixels pos (Size 1 1) (PixelData DepthComponent GL.Float ptr)
   [winZ] <- peekArray 1 ptr

   -- TODO: force arr before freeing?
   Ptr.free ptr

   return winZ


-- | Get model coordinate for given window position.
getModelCoord :: Position -> IO (Vertex3 GLdouble)
getModelCoord pos@(Position posX posY) = do
   posZ       <- getWindowZ pos
   modelView  <- getModelView 0
   projection <- get (GL.matrix $ Just GL.Projection)
   view       <- get GL.viewport
   let winX = fromIntegral posX
   let winY = fromIntegral posY
   let winZ = realToFrac posZ
   GL.unProject (Vertex3 winX winY winZ) modelView projection view


-- | Get model position for given mouse position.
getModelPosition :: Position -> IO P3
getModelPosition mousePos = do
   pos <- flipPosition mousePos
   vertex3ToP3 <$> getModelCoord pos


vertex3ToP3 :: RealFrac a => Vertex3 a -> P3
vertex3ToP3 (Vertex3 x y z) = P3 (floor (x/2)) (floor (y/2)) (round z)


getModelView :: GLsizei -> IO (GLmatrix GLdouble)
getModelView vertexUnit = get (GL.matrix $ Just $ GL.Modelview vertexUnit)
