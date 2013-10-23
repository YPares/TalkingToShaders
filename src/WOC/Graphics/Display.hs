module WOC.Graphics.Display where

import qualified Graphics.Rendering.OpenGL.Raw as G
import qualified Graphics.Rendering.TalkingToShaders as S
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.GLFW as W

import Control.Monad

openDisplay :: IO ()
openDisplay = do
  W.initialize
  success <- W.openWindow $ W.defaultDisplayOptions
    { W.displayOptions_numFsaaSamples = Just 4}
  when (not success) $ do
    W.terminate
    error "GLFW couldn't open a window."
  
displayOneFrame :: IO ()
displayOneFrame = return ()

