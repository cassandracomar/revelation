module Revelation.Utils where
import Pipes
import Linear
import Revelation.Mat
import Revelation.Core
import Control.Monad
import Control.Lens

indexP :: V2 Int -> Pipe (Mat Grayscale Int) (Mat Grayscale Int) CV ()
indexP i = forever $ do 
              m <- await
              m' <- lift $ return m & pixel i .~ return (V1 0)
              v <- lift $ return m' ^. pixel i
              liftCV $ print (show v)
              yield m
