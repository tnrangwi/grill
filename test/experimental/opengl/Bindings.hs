module Bindings
(
 idle
, display
, reshape
, keyboardMouse
, menuCall
)
where

import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as CTrans
import qualified Graphics.Rendering.OpenGL.GL.StateVar as StateVar
import qualified Graphics.UI.GLUT.Callbacks.Window as Window
import qualified Graphics.UI.GLUT.Begin as Begin
import Graphics.Rendering.OpenGL.GL.StateVar (($=))

import Display
import Menu

reshape :: CTrans.Size
        -> IO ()
reshape s@(CTrans.Size _ _) = do -- Size w h
  CTrans.viewport $= (CTrans.Position 0 0, s)

keyboardAct :: (StateVar.HasGetter s1,
                StateVar.HasGetter s,
                StateVar.HasSetter s1,
                StateVar.HasSetter s,
                Fractional t,
                Fractional t1,
                Fractional a) =>
               s a
            -> s1 (t, t1)
            -> Window.Key
            -> Window.KeyState
            -> IO ()

keyboardAct a _ (Window.Char ' ') Window.Down = do
  a' <- StateVar.get a
  a $= -a'

keyboardAct a _ (Window.Char '+') Window.Down = do
  a' <- StateVar.get a
  a $= 2*a'

keyboardAct a _ (Window.Char '-') Window.Down = do
  a' <- StateVar.get a
  a $= a'/2

keyboardAct _ p (Window.SpecialKey Window.KeyLeft) Window.Down = do
  (x, y) <- StateVar.get p
  p $= (x-0.1,y)

keyboardAct _ p (Window.SpecialKey Window.KeyRight) Window.Down = do
  (x, y) <- StateVar.get p
  p $= (x+0.1,y)

keyboardAct _ p (Window.SpecialKey Window.KeyUp) Window.Down = do
  (x, y) <- StateVar.get p
  p $= (x,y+0.1)

keyboardAct _ p (Window.SpecialKey Window.KeyDown) Window.Down = do
  (x, y) <- StateVar.get p
  p $= (x,y-0.1)

keyboardAct _ _ (Window.Char 'q') Window.Down = do
  Begin.leaveMainLoop

keyboardAct _ _ _ _ = return ()
keyboardMouse :: (StateVar.HasGetter s,
                  StateVar.HasGetter s1,
                  StateVar.HasSetter s,
                  StateVar.HasSetter s1,
                  Fractional t3,
                  Fractional a,
                  Fractional t2) =>
                 s a
              -> s1 (t2, t3)
              -> Window.Key
              -> Window.KeyState
              -> t
              -> t1
              -> IO ()
keyboardMouse angle pos key state _ _ = do
  keyboardAct angle pos key state

menuCall onOff pos = do
  print "menu opened"
  -- print $ "Menu on / off at position:" ++ show pos ++ "(" ++ show onOff ++ ")"
