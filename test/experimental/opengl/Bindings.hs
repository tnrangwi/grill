module Bindings
(
 idle
, display
, reshape
, keyboardMouse
)
where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Display

reshape :: Size
        -> IO ()
reshape s@(Size _ _) = do -- Size w h
  viewport $= (Position 0 0, s)

keyboardAct :: (HasGetter s1,
                HasGetter s,
                HasSetter s1,
                HasSetter s,
                Fractional t,
                Fractional t1,
                Fractional a) =>
               s a
            -> s1 (t, t1)
            -> Key
            -> KeyState
            -> IO ()

keyboardAct a _ (Char ' ') Down = do
  a' <- get a
  a $= -a'

keyboardAct a _ (Char '+') Down = do
  a' <- get a
  a $= 2*a'

keyboardAct a _ (Char '-') Down = do
  a' <- get a
  a $= a'/2

keyboardAct _ p (SpecialKey KeyLeft) Down = do
  (x, y) <- get p
  p $= (x-0.1,y)

keyboardAct _ p (SpecialKey KeyRight) Down = do
  (x, y) <- get p
  p $= (x+0.1,y)

keyboardAct _ p (SpecialKey KeyUp) Down = do
  (x, y) <- get p
  p $= (x,y+0.1)

keyboardAct _ p (SpecialKey KeyDown) Down = do
  (x, y) <- get p
  p $= (x,y-0.1)

keyboardAct _ _ _ _ = return ()
keyboardMouse :: (HasGetter s,
                  HasGetter s1,
                  HasSetter s,
                  HasSetter s1,
                  Fractional t3,
                  Fractional a,
                  Fractional t2) =>
                 s a
              -> s1 (t2, t3)
              -> Key
              -> KeyState
              -> t
              -> t1
              -> IO ()
keyboardMouse angle pos key state _ _ = do
  keyboardAct angle pos key state
