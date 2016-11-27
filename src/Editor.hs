{-# LANGUAGE TemplateHaskell #-}

module Editor where

import Model
import ModelRender (renderModel)
import Types

import Graphics.Rendering.OpenGL as GL
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH

--------------------------------------------------------------------------------

data Editor
   = Place [PlacedPart] [PlacedPart]
   | Parts [PlacedPart]
   deriving Show

placedParts :: Editor -> [PlacedPart]
placedParts (Place _ placedParts) = placedParts
placedParts (Parts ps) = ps

partsToPlace :: Editor -> [PlacedPart]
partsToPlace (Place ps _) = ps
partsToPlace (Parts ps)   = ps

--------------------------------------------------------------------------------

-- Not used now -- should possibly not be in this module
renderEditor :: Editor -> IO ()
renderEditor (Place toPlace placed) = do

   -- Render wireframe for parts to place
   GL.polygonMode $= (GL.Line, GL.Line)
   renderModel toPlace

   -- Render placed parts "normally"
   GL.polygonMode $= (GL.Fill, GL.Fill)
   renderModel placed

renderEditor (Parts parts) = do
   GL.polygonMode $= (GL.Fill, GL.Fill)
   renderModel parts

--------------------------------------------------------------------------------
