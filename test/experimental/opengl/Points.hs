-- | Example for test purposes - copied from http://www.haskell.org/haskellwiki/OpenGLTutorial1.
-- For all rights see there - I just do some tests.

module Points where

import qualified Graphics.Rendering.OpenGL.GL.BasicTypes as GLBase

points :: Int -> [(GLBase.GLfloat, GLBase.GLfloat, GLBase.GLfloat)]
points n' = let n = fromIntegral n' in map (\k -> let t = 2*pi*k/n in (sin(t), cos(t), 0.0)) [1..n]
