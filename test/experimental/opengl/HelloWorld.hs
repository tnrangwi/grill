-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

--import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=))

import qualified Data.IORef as IORef

import qualified Bindings

-- myPoints :: [(GLfloat, GLfloat, GLfloat)]
-- myPoints = map (\k -> (sin(2*pi*k/12), cos(2*pi**k/12), 0.0))[1..12]

main :: IO ()
main = do
  (progname, _) <- GLUT.getArgsAndInitialize -- GLUT.Initialitialization
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered] -- GLUT.Initialization
  _ <- GLUT.createWindow progname -- GLUT.Window
  GLUT.reshapeCallback $= Just Bindings.reshape -- GLUT.Callbacks.Window
  angle <- IORef.newIORef 0.0
  delta <- IORef.newIORef (0.1::GLUT.GLfloat) -- GLUT
  position <- IORef.newIORef (0.0::GLUT.GLfloat, 0.0)
  GLUT.keyboardMouseCallback $= Just (Bindings.keyboardMouse delta position) -- GLUT.Callbacks.Window
  GLUT.idleCallback $= Just (Bindings.idle angle delta) -- GLUT.Callbacks.Global
  GLUT.displayCallback $= (Bindings.display angle position) -- GLUT.Callbacks.Window
  GLUT.mainLoop -- GLUT.Begin
