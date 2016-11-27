-- | Colors for openGL

module GLColor where

import Graphics.Rendering.OpenGL.GL (GLfloat)
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color3(..))

red, green, blue, yellow, tan, lightBlue, black, white :: Color3 GLfloat

red = Color3 1 0 0

green = Color3 0 0.8 0

blue = Color3 0 0 1

yellow = Color3 1 1 0

tan = Color3 1 0.9 0.5

lightBlue = Color3 0.45 0.66 0.89

black = Color3 0.1 0.1 0.1

white = Color3 1 1 1
