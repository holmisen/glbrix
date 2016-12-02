{-# LANGUAGE TemplateHaskell #-}

module App where

import Camera
import Command
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
  , _appCurrentColor :: IORef Types.Color
  }

makeLenses ''App


newDefaultApp :: IO App
newDefaultApp = do
   cam <- newIORef defaultCamera
   editor <- newIORef $ startEditor
   color <- newIORef $ Types.Red
   return App
      { _appCamera = cam
      , _appProjection = Ortho
      , _appEditor = editor
      , _appCurrentColor = color
      }

--------------------------------------------------------------------------------

startPlate = part (Plate 20 20) (P3 (-10) (-10) 0) noRotation Types.Green

startBrick = part (Brick 2 2) (P3 0 0 1) noRotation Types.Red

startEditor = Editor.Place [startBrick] [startPlate]

--------------------------------------------------------------------------------

execCommand :: App -> Command -> IO ()
execCommand app cmd = do
   color <- get (_appCurrentColor app)
   case cmd of
      CmdPlate l w ->
         let plate = part (Plate l w) (P3 0 0 0) noRotation color
         in _appEditor app $~ Editor.placeNewParts [plate]
      CmdBrick l w ->
         let brick = part (Brick l w) (P3 0 0 0) noRotation color
         in _appEditor app $~ Editor.placeNewParts [brick]
      CmdColor c -> do
         _appCurrentColor app $= c
         _appEditor app $~ Editor.setSelectedPartsColor c
      CmdClone ->
         _appEditor app $~ Editor.cloneSelectedParts
