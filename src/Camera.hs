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

defaultCamera =
  Polar { _camDistance = 50
        , _camElevation = 30
        , _camAzimuth = 0
        }


-- _camAzimuth :: Lens' Camera GLdouble
-- _camAzimuth = lens camAzimuth (\s x -> s { camAzimuth = x })

-- _camElevation :: Lens' Camera GLdouble
-- _camElevation = lens camElevation (\s x -> s { camElevation = x })

-- _camDistance :: Lens' Camera GLdouble
-- _camDistance = lens camDistance (\s x -> s { camDistance = x })


applyCamera (Polar azimuth elevation distance) = do
  GL.loadIdentity
  polarView distance elevation azimuth

  -- Translate scene downwards to make origo be lower
  GL.translate (Vector3 0 0 (-10::GLfloat))


polarView :: GLdouble -> GLdouble -> GLdouble -> IO ()
polarView distance elevation azimuth = do
  GL.translate (Vector3 0 0 (-distance))
  GL.rotate (-elevation) (Vector3 1 0 0)
  GL.rotate  azimuth     (Vector3 0 0 1)
