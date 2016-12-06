{-# LANGUAGE TemplateHaskell #-}

module App where

import Camera
import Command
import Editor
import Model
import Primitive
import Types

import Control.Arrow ((>>>))
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
  { _appCamera       :: IORef Camera
  , _appProjection   :: ProjectionType
  , _appEditor       :: IORef Editor
  , _appCurrentColor :: IORef Types.Color
  }

makeLenses ''App


newDefaultApp :: IO App
newDefaultApp = do
   cam    <- newIORef defaultCamera
   editor <- newIORef startEditor
   color  <- newIORef Types.Tan
   return App
      { _appCamera = cam
      , _appProjection = Ortho
      , _appEditor = editor
      , _appCurrentColor = color
      }

--------------------------------------------------------------------------------

startPlate = part (Plate 20 20) (P3 (-10) (-10) 0) noRotation Types.Green

startBrick = part (Brick 2 2) (P3 0 0 1) noRotation Types.Tan

startEditor = Editor.placeNewParts [startBrick] $ Editor.makeEditor [startPlate]

--------------------------------------------------------------------------------

execCommand :: App -> Command -> IO ()
execCommand app cmd = do
   color <- get (_appCurrentColor app)
   let newPart p = part p origo noRotation color
   case cmd of
      CmdBlock l w h
         | h == 0    -> insert $ newPart (Plate l w)
         | otherwise -> insert $ newPart (Block l w h)
      CmdPlate l w ->
         insert $ newPart (Plate l w)
      CmdBrick l w ->
         insert $ newPart (Brick l w)
      CmdColor c -> do
         _appCurrentColor app $= c
         _appEditor app $~ Editor.setSelectedPartsColor c
      CmdClone ->
         _appEditor app $~ Editor.cloneSelectedParts
      CmdDelete ->
         _appEditor app $~ Editor.deleteSelected
      CmdMove ->
         _appEditor app $~ Editor.placeSelectedParts
      CmdGroup ->
         _appEditor app $~ (Editor.groupSelectedParts >>>
                            Editor.unselectAll)
      CmdUngroup ->
         _appEditor app $~ Editor.ungroupSelectedParts
      CmdRotate ->
         _appEditor app $~ Editor.rotateSelectedParts
   where
      insert part = _appEditor app $~ Editor.placeNewParts [part]
