-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Bindings

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12), cos(2*pi**k/12), 0.0))[1..12]

main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello, world!"
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop

