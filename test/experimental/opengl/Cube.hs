-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

module Cube
where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

cube w = do
  renderPrimitive Quads $ do
           vertex $ Vertex3 w w w
           vertex $ Vertex3 w w (-w)
           vertex $ Vertex3 w (-w) (-w)
           vertex $ Vertex3 w (-w) w
           vertex $ Vertex3 w w w
           vertex $ Vertex3 w w (-w)
           vertex $ Vertex3 (-w) w (-w)
           vertex $ Vertex3 (-w) w w
           vertex $ Vertex3 w w w
           vertex $ Vertex3 w (-w) w
           vertex $ Vertex3 (-w) (-w) w
           vertex $ Vertex3 (-w) w w
           vertex $ Vertex3 (-w) w w
           vertex $ Vertex3 (-w) w (-w)
           vertex $ Vertex3 (-w) (-w) (-w)
           vertex $ Vertex3 (-w) (-w) w
           vertex $ Vertex3 w (-w) w
           vertex $ Vertex3 w (-w) (-w)
           vertex $ Vertex3 (-w) (-w) (-w)
           vertex $ Vertex3 (-w) (-w) w
           vertex $ Vertex3 w w (-w)
           vertex $ Vertex3 w w (-w)
           vertex $ Vertex3 w (-w) (-w)
           vertex $ Vertex3 (-w) (-w) (-w)
           vertex $ Vertex3 (-w) w (-w)
