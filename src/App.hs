{-# LANGUAGE TemplateHaskell #-}

module App where

import Camera
import Editor
import Model
import Primitive
import Types

import Data.Foldable (for_)
import Data.IORef
import Graphics.Rendering.OpenGL as GL
import Lens.Micro
import Lens.Micro.TH

--------------------------------------------------------------------------------

data ProjectionType = Ortho
  deriving (Eq, Ord, Show)

data App =
  App
  { _appCamera     :: IORef Camera
  , _appProjection :: ProjectionType
  , _appEditor     :: IORef Editor
  }

makeLenses ''App


newDefaultApp :: IO App
newDefaultApp = do
   cam <- newIORef defaultCamera
   editor <- newIORef $ startEditor
   return App
      { _appCamera = cam
      , _appProjection = Ortho
      , _appEditor = editor
      }

--------------------------------------------------------------------------------

startPlate = part (Plate 20 20) (P3 (-10) (-10) 0) noRotation (Just Types.Green)

startBrick = part (Brick 2 2) (P3 0 0 1) noRotation (Just Types.Red)

startEditor = Editor.Place [startBrick] [startPlate]

--------------------------------------------------------------------------------
