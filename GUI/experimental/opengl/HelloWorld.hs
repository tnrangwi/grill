-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello, world!"
  displayCallback $= display
  mainLoop

display = do
  clear [ ColorBuffer ] ; flush
