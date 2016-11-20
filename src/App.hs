{-# LANGUAGE TemplateHaskell #-}

module App where

import Camera
import Model

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
  , _appModel      :: IORef [PlacedPart]
  }

makeLenses ''App


newDefaultApp = do
   cam <- newIORef defaultCamera
   model <- newIORef $ Model.example
   return App
      { _appCamera = cam
      , _appProjection = Ortho
      , _appModel = model
      }
