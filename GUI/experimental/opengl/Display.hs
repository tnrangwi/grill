module Display
(
 display
)

where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Cube

display = do
    clear [ ColorBuffer ]
    cube (0.2::GLfloat)
    flush
