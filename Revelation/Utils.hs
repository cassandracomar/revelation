module Revelation.Utils where
import Pipes
import Linear
import Revelation.Mat
import Revelation.Core
import Control.Monad
import Control.Lens

indexP :: Pipe (Mat Grayscale Int) (Mat Grayscale Int) CV ()
indexP = forever $ do 
            m <- await
            m' <- lift $ return m & pixel (V2 0 0) .~ (return (V1 255))
            v <- lift $ return m' ^. pixel (V2 0 0) 
            liftCV $ print v
            yield m
