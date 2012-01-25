-- | Example for test purposes - enhancing code copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - this code adds a menu.
module Menu
(
 main
)
where

import qualified Graphics.UI.GLUT as GLUT


test = do
  print "test"


main = (GLUT.Menu [GLUT.MenuEntry "Console output (test)" test,
                   GLUT.MenuEntry "Quit" GLUT.leaveMainLoop])
