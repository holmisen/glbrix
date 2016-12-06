{-# LANGUAGE TemplateHaskell #-}

module Camera where

import Graphics.Rendering.OpenGL as GL
import Lens.Micro
import Lens.Micro.TH


data Camera =
  Polar
  { _camAzimuth   :: GLdouble
  , _camElevation :: GLdouble
  , _camDistance  :: GLdouble
  }
  deriving (Eq, Ord, Show)

makeLenses ''Camera

defaultCamera :: Camera
defaultCamera =
  Polar { _camDistance = 50
        , _camElevation = 30
        , _camAzimuth = 0
        }


applyCamera :: Camera -> IO ()
applyCamera (Polar azimuth elevation distance) = do
  GL.loadIdentity
  polarView distance elevation azimuth

  -- Translate scene downwards to make origo be lower
  GL.translate (Vector3 0 0 (-13::GLfloat))


polarView :: GLdouble -> GLdouble -> GLdouble -> IO ()
polarView distance elevation azimuth = do
  GL.translate (Vector3 0 0 (-distance))
  GL.rotate (-elevation) (Vector3 1 0 0)
  GL.rotate  azimuth     (Vector3 0 0 1)
