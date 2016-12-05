-- | Colors for openGL

module GLColor where

import Graphics.Rendering.OpenGL.GL (GLfloat)
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color3(..))

black, blue, brown, darkBlue, darkGray, darkGreen, gray, green, lightBlue, red, tan, white, yellow :: Color3 GLfloat

black     = Color3 0.1  0.1  0.1
blue      = Color3 0    0    1
brown     = Color3 0.5  0.25 0
darkBlue  = Color3 0.02 0.02 0.5
darkGray  = Color3 0.5  0.5  0.5
darkGreen = Color3 0    0.3  0
gray      = Color3 0.8  0.8  0.8
green     = Color3 0    0.8  0
lightBlue = Color3 0.45 0.66 0.89
red       = Color3 1    0    0
tan       = Color3 1    0.9  0.5
white     = Color3 1    1    1
yellow    = Color3 1    1    0
wireframe = Color3 0.2  0.2  0.2
