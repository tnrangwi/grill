-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

module Display
(
 display
, idle
)

where

import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as CTrans
import qualified Graphics.Rendering.OpenGL.GL.BasicTypes as GLBase
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer as FBuffer
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec as VSpec
import qualified Graphics.Rendering.OpenGL.GL.StateVar as StateVar
import Graphics.Rendering.OpenGL.GL.StateVar (($=!))
import qualified Graphics.UI.GLUT.Window as Window

import qualified Cube
import qualified Points

display :: (StateVar.HasGetter g1, StateVar.HasGetter g, CTrans.MatrixComponent c, Num c) =>
           g1 GLBase.GLfloat
        -> g (c, c)
        -> IO ()
display angle position = do
  FBuffer.clear [ FBuffer.ColorBuffer ]
  CTrans.loadIdentity
  (x, y) <- StateVar.get position
  CTrans.translate $ CTrans.Vector3 x y 0
  CTrans.preservingMatrix $ do
    a <- StateVar.get angle
    CTrans.rotate a $ CTrans.Vector3 0 0 (1::GLBase.GLfloat)
    CTrans.scale 0.7 0.7 (0.7::GLBase.GLfloat)
    mapM_ (\(x, y, z) -> CTrans.preservingMatrix $ do
                           VSpec.color $ VSpec.Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
                           CTrans.translate $ CTrans.Vector3 x y z
                           Cube.cube (0.1::GLBase.GLfloat)
          ) $ Points.points 7
  Window.swapBuffers

idle :: (StateVar.HasGetter g, StateVar.HasGetter s, StateVar.HasSetter s, Num a) =>
        s a
     -> g a
     -> IO ()
idle angle delta = do
  a <- StateVar.get angle
  d <- StateVar.get delta
  angle $=! (a + d)
  Window.postRedisplay Nothing
