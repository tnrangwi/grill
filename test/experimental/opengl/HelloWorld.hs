-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.IORef

import Bindings

-- myPoints :: [(GLfloat, GLfloat, GLfloat)]
-- myPoints = map (\k -> (sin(2*pi*k/12), cos(2*pi**k/12), 0.0))[1..12]

main = do
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello, world!"
  reshapeCallback $= Just reshape
  angle <- newIORef 0.0
  delta <- newIORef (0.1::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0)
  keyboardMouseCallback $= Just (keyboardMouse delta position)
  idleCallback $= Just (idle angle delta)
  displayCallback $= (display angle position)
  mainLoop

