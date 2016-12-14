import Control.Monad
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
import Command
import Editor
import GLUtils
import Model
import ModelRender
import Primitive (Prim)
import Types
import qualified Primitive

--------------------------------------------------------------------------------

logInfo = putStrLn

backgroundColor = Color4 0.64 0.8 1.0 1

--------------------------------------------------------------------------------

main :: IO ()
main = do
  GLUT.getArgsAndInitialize

  app <- newDefaultApp

  GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered, GLUT.WithDepthBuffer]

  GLUT.createWindow "glbrix"

  logInfo =<< get GLUT.glutVersion

  -- This must be located *after* the call to createWindow.
  GL.depthFunc $= Just Less

  GLUT.reshapeCallback $= Just (reshape app)

  GLUT.displayCallback $= display app

  mousePos <- newIORef Nothing
  camRef   <- newIORef (undefined :: Camera)
  keysRef  <- newIORef []

  GLUT.mouseCallback $=
     (Just $ \ btn keyState p -> do
           when (btn == RightButton) $
              case keyState of
                 Down -> do
                    mousePos $=! Just p
                    (camRef $=!) =<< get (_appCamera app)
                 Up ->
                    mousePos $=! Nothing
           handleMouse app btn keyState p)

  GLUT.motionCallback $=
     (Just $ \(Position x y) -> do
           refPosition <- get mousePos
           case refPosition of
              Nothing -> return ()
              Just (Position x' y') -> do
                 cam <- get camRef
                 let dx = fromIntegral (x - x')
                 let dy = fromIntegral (y - y')
                 let newCam = ((camElevation %~ \e -> e - dy) .
                               (camAzimuth   %~ \a -> a + dx)) cam
                 _appCamera app $=! newCam
                 GLUT.postRedisplay Nothing
     )

  GLUT.passiveMotionCallback $= Just (handleMouseMove app)


  -- When keys are pressed, their characters are accumulated into
  -- commands than can be executed.
  GLUT.keyboardCallback $=
     (Just $ \char mousePos -> do
           case char of
              '\b'   -> keysRef $~ drop 1
              '\ESC' -> do
                 keysRef $= []
                 _appEditor app $~ Editor.escapeEdit
                 GLUT.postRedisplay Nothing
              '?' -> do
                 editor <- get (_appEditor app)
                 let n = length $ concatMap flattenPartTree $ allParts editor
                 logInfo $ "Number of pieces: " ++ show n
              _ -> do
                 keysRef $~ (char :)
                 keys <- get keysRef
                 case Command.readCommand (reverse keys) of
                    Nothing -> return ()
                    Just cmd -> do
                       logInfo $ show cmd
                       App.execCommand app cmd
                       keysRef $= []
                       GLUT.postRedisplay Nothing
           cmdBuf <- get keysRef
           logInfo $ "Command buffer: " ++ cmdBuf
     )

  GLUT.specialCallback $=
     (Just $ \key mousePos -> do
           case key of
              KeyUp    -> _appEditor app $~ Editor.moveSelectedParts (Vector3   0   1  0)
              KeyDown  -> _appEditor app $~ Editor.moveSelectedParts (Vector3   0 (-1) 0)
              KeyLeft  -> _appEditor app $~ Editor.moveSelectedParts (Vector3 (-1)  0  0)
              KeyRight -> _appEditor app $~ Editor.moveSelectedParts (Vector3   1   0  0)
              otherwise -> return ()
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

  let h_model = 25
  let scale   = h_model / h'
  let w_model = w' * scale

  let w2 = w_model / 2
  let h2 = h_model / 2

--  GL.ortho (-w2) w2 (-h2) h2 1 500

  GL.frustum (-w2) w2 (-h2) h2 20 800

  GL.matrixMode $= Modelview 0


display :: App -> IO ()
display app = do
  GL.clearColor $= backgroundColor

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  applyCamera =<< get (_appCamera app)

  GL.cullFace $= Just (GL.Back)

  editor <- get (_appEditor app)

  renderAxis 2

  renderModel $ editor ^. lnonSelectedParts

  -- Render "invisible" plane for picking
  renderBackgroundPlane (-500, -500) (500, 500)

  -- TODO: Save the depth buffer at this point for later picking

  renderModelWireframe $ editor ^. lselectedParts

  GLUT.swapBuffers


renderBackgroundPlane :: (GLint, GLint) -> (GLint, GLint) -> IO ()
renderBackgroundPlane (xMin, yMin) (xMax, yMax) = do
   GL.color backgroundColor
   GL.polygonMode $= (GL.Fill, GL.Fill)
   GL.polygonOffsetFill $= Enabled
   GL.renderPrimitive GL.Quads $ do
      GL.vertex $ GL.Vertex2 xMin yMin
      GL.vertex $ GL.Vertex2 xMax yMin
      GL.vertex $ GL.Vertex2 xMax yMax
      GL.vertex $ GL.Vertex2 xMin yMax
   GL.polygonOffsetFill $= Disabled

--------------------------------------------------------------------------------
handleMouse :: App -> MouseCallback

handleMouse app LeftButton Down mousePos = do
  applyCamera =<< get (_appCamera app)

  editor <- get (_appEditor app)

  case editor of
     Place {} ->
        _appEditor app $= Editor.placeParts editor

     Pick {} -> do
        let placed = editor ^. lnonSelectedParts
        partMaybe <- pickPlacedPart mousePos (Editor.allParts editor)
        case partMaybe of
           Nothing ->
              _appEditor app $= Editor.unselectAll editor
           Just partIndex ->
              _appEditor app $= Editor.toggleSelected partIndex editor

  GLUT.postRedisplay Nothing

handleMouse app WheelUp Down mousePos = do
   _appCamera app $~ (camDistance %~ (\d -> d - 5))
   GLUT.postRedisplay Nothing

handleMouse app WheelDown Down mousePos = do
   _appCamera app $~ (camDistance %~ (\d -> d + 5))
   GLUT.postRedisplay Nothing

handleMouse _ _ _ _ =
  return ()

--------------------------------------------------------------------------------

handleMouseMove :: App -> MotionCallback
handleMouseMove app mousePos = do
   modelPos <- GLUtils.getModelPosition mousePos
--   logInfo $ "Pos: " ++ show modelPos
   editor <- get (_appEditor app)
   case editor of
      Place {} -> do
         _appEditor app $= Editor.moveParts modelPos editor
         GLUT.postRedisplay Nothing
      _other ->
         return ()

--------------------------------------------------------------------------------

-- | Return the index of the placed part under given position, or
-- Nothing if there is no part under position.
pickPlacedPart :: Position -> [PlacedPart] -> IO (Maybe Int)
pickPlacedPart = pickPart ModelRender.renderPart


pickPart :: Renderer a -> Position -> [a] -> IO (Maybe Int)
pickPart renderer pos parts = do
  GL.clearColor $= Color4 1 1 1 (1 :: GLfloat)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  renderWithUniqColors renderer parts
  GL.flush

  color <- getColorAtPosition pos
  let partIndex = if color < maxBound then Just (fromEnum $ looseAlpha color) else Nothing
  logInfo $ show pos ++ "\t" ++ show color ++ "\t (part " ++ show partIndex ++ ")"
  return partIndex


getColorAtPosition :: Position -> IO (Color4 GLubyte)
getColorAtPosition pos@(Position posX posY) = do
  -- glFlush()
  -- glFinish()

  GL.readBuffer $= BackBuffers

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
