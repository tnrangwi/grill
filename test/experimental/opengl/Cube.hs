-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

module Cube

where

import qualified Graphics.Rendering.OpenGL.GL.VertexSpec as VSpec
import qualified Graphics.Rendering.OpenGL.GL.BeginEnd as GLBegEnd

cube :: (VSpec.VertexComponent a, Num a) => a 
     -> IO ()
cube w = do
  GLBegEnd.renderPrimitive GLBegEnd.Quads $ do
           VSpec.vertex $ VSpec.Vertex3 w w w
           VSpec.vertex $ VSpec.Vertex3 w w (-w)
           VSpec.vertex $ VSpec.Vertex3 w (-w) (-w)
           VSpec.vertex $ VSpec.Vertex3 w (-w) w
           VSpec.vertex $ VSpec.Vertex3 w w w
           VSpec.vertex $ VSpec.Vertex3 w w (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) w (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) w w
           VSpec.vertex $ VSpec.Vertex3 w w w
           VSpec.vertex $ VSpec.Vertex3 w (-w) w
           VSpec.vertex $ VSpec.Vertex3 (-w) (-w) w
           VSpec.vertex $ VSpec.Vertex3 (-w) w w
           VSpec.vertex $ VSpec.Vertex3 (-w) w w
           VSpec.vertex $ VSpec.Vertex3 (-w) w (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) (-w) (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) (-w) w
           VSpec.vertex $ VSpec.Vertex3 w (-w) w
           VSpec.vertex $ VSpec.Vertex3 w (-w) (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) (-w) (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) (-w) w
           VSpec.vertex $ VSpec.Vertex3 w w (-w)
           VSpec.vertex $ VSpec.Vertex3 w w (-w)
           VSpec.vertex $ VSpec.Vertex3 w (-w) (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) (-w) (-w)
           VSpec.vertex $ VSpec.Vertex3 (-w) w (-w)
