import Data.IORef
import Data.StateVar
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT          as GLUT
import Lens.Micro
import Foreign.Marshal.Alloc as Ptr
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Ptr           (Ptr)

import App
import Camera
import Model
import ModelRender
import Primitive (Prim)
import Types
import qualified Primitive


main = do
  GLUT.getArgsAndInitialize

  app <- newDefaultApp

  GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered, GLUT.WithDepthBuffer]

  GLUT.createWindow "glblocks"

  -- This must be located *after* the call to createWindow.
  GL.depthFunc $= Just Less

  GLUT.reshapeCallback $= Just (reshape app)

  GLUT.displayCallback $= display app

  mousePos <- newIORef (undefined :: Position)
  camRef   <- newIORef (undefined :: Camera)

  GLUT.mouseCallback $=
     (Just $ \ btn keyState p -> do
           mousePos $=! p
           (camRef   $=!) =<< get (_appCamera app)
           handleMouse app btn keyState p)

  GLUT.motionCallback $=
     (Just $ \(Position x y) -> do
           Position x' y' <- get mousePos
           cam <- get camRef
           let dx = fromIntegral (x - x')
           let dy = fromIntegral (y - y')
           let newCam = ((camElevation %~ \e -> e - dy) .
                         (camAzimuth   %~ \a -> a + dx)) cam
           _appCamera app $=! newCam
           GLUT.postRedisplay Nothing
     )

  GLUT.mainLoop


reshape :: App -> GLUT.Size -> IO ()
reshape app (Size w h) = do
  GL.viewport $= (Position 0 0, Size w h)

  GL.matrixMode $= Projection

  GL.loadIdentity

  let w' = fromIntegral w
  let h' = fromIntegral h

  let w_model = 50
  let scale   = w_model / w'
  let h_model = h' * scale

  let w2 = w_model / 2
  let h2 = h_model / 2

--  GL.ortho (-w2) w2 (-h2) h2 1 500

  GL.frustum (-w2) w2 (-h2) h2 20 70

  GL.matrixMode $= Modelview 0


display :: App -> IO ()
display app = do
  GL.clearColor $= Color4 0.64 0.8 1 1

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  applyCamera =<< get (_appCamera app)

--  GL.currentColor $= Color3 1 0 0

  -- Enable this to draw wireframe:
--  GL.polygonMode $= (GL.Line, GL.Line)

  renderAxis 2
  GL.cullFace $= Just (GL.Back)
  renderModel =<< get (_appModel app)

  GLUT.swapBuffers


handleMouse :: App -> MouseCallback
handleMouse app LeftButton Down pos = do
  applyCamera =<< get (_appCamera app)
  parts <- get (_appModel app)
  pickPlacedPart pos parts
  return ()

handleMouse _ _ _ _ =
  return ()


-- | Return the index of the placed part under given position, or
-- Nothing if there is no part under position.
pickPlacedPart :: Position -> [PlacedPart] -> IO (Maybe Int)
pickPlacedPart = pickPart renderPart

-- | Return the z position of the top of the part under given position
-- or 0 if there is no part under position.
pickPlacedPrim :: Position -> [PlacedPart] -> IO Double
pickPlacedPrim pos parts = do
   let prims = concatMap flattenPartTree parts
   index <- pickPart renderPlaced pos prims
   case index of
      Nothing -> return 0
      Just i  ->
         let
            placed   = prims !! i
            P3 _ _ z = placed ^. (lplacement.lposition)
            prim     = placed ^. lplacedValue
         in
            return $ fromIntegral z + Primitive.height prim


pickPart :: Renderer a -> Position -> [a] -> IO (Maybe Int)
pickPart renderer pos parts = do
  GL.clearColor $= Color4 1 1 1 (1 :: GLfloat)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  renderWithUniqColors renderer parts
  GL.flush

  color <- getColorAtPosition pos
  let partIndex = if color < maxBound then Just (fromEnum $ looseAlpha color) else Nothing
  putStrLn $ show pos ++ "\t" ++ show color ++ "\t (part " ++ show partIndex ++ ")"
  return partIndex


getColorAtPosition :: Position -> IO (Color4 GLubyte)
getColorAtPosition pos@(Position posX posY) = do
  -- glFlush()
  -- glFinish()

--  GL.readBuffer $= FrontBuffers

  GL.rowAlignment Unpack $= 1

  -- Flip y coordinate
  (Size w h) <- get GLUT.windowSize
  let flippedPos = Position posX (fromIntegral h - posY)

  ptr <- mallocArray 4 :: IO (Ptr GLubyte)
  GL.readPixels flippedPos (Size 1 1) (PixelData RGBA UnsignedByte ptr)
  [r,g,b,a] <- peekArray 4 ptr

  -- TODO: force arr before freeing?
  Ptr.free ptr

  return (Color4 r g b a)

--------------------------------------------------------------------------------

looseAlpha :: Color4 a -> Color3 a
looseAlpha (Color4 r g b a) = Color3 r g b
