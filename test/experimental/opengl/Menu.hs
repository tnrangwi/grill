-- | Example for test purposes - enhancing code copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - this code adds a menu.
module Menu
(
 main
)
where

import qualified Graphics.UI.GLUT.Menu as Menu
import qualified Graphics.UI.GLUT.Begin as Begin

test = do
  print "test"


main = (Menu.Menu [Menu.MenuEntry "Console output (test)" test,
                   Menu.MenuEntry "Quit" Begin.leaveMainLoop])
