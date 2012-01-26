-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

import Graphics.Rendering.OpenGL.GL.BasicTypes as GLBase
import Graphics.Rendering.OpenGL.GL.StateVar (($=))
import qualified Graphics.UI.GLUT.Initialization as GLuInit
import qualified Graphics.UI.GLUT.Window as Window
import qualified Graphics.UI.GLUT.Callbacks.Window as WCallbacks
import qualified Graphics.UI.GLUT.Callbacks.Global as GCallbacks
import qualified Graphics.UI.GLUT.Menu as GLMenu
import qualified Graphics.UI.GLUT.Begin as Begin

import qualified Data.IORef as IORef

import qualified Bindings
import qualified Menu


main :: IO ()
main = do
  (progname, _) <- GLuInit.getArgsAndInitialize
  GLuInit.initialDisplayMode $= [GLuInit.DoubleBuffered]
  _ <- Window.createWindow progname
  WCallbacks.reshapeCallback $= Just Bindings.reshape
  angle <- IORef.newIORef 0.0
  delta <- IORef.newIORef (0.1::GLBase.GLfloat)
  position <- IORef.newIORef (0.0::GLBase.GLfloat, 0.0)
  WCallbacks.keyboardMouseCallback $= Just (Bindings.keyboardMouse delta position)
  GCallbacks.idleCallback $= Just (Bindings.idle angle delta)
  WCallbacks.displayCallback $= (Bindings.display angle position)
  GLMenu.attachMenu WCallbacks.LeftButton Menu.main
  GCallbacks.menuStatusCallback $= Just Bindings.menuCall -- does not work
  Begin.mainLoop
