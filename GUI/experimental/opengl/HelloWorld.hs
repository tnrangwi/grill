-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12), cos(2*pi**k/12), 0.0))[1..12]

main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello, world!"
  displayCallback $= display
  reshapeCallback $= Just reshape
  mainLoop

display = do
  clear [ ColorBuffer ]
  renderPrimitive Points $ mapM_(\(x, y, z) -> vertex$Vertex3 x y z) myPoints
  flush

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing
