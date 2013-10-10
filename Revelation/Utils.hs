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
            v <- lift $ return m ^. neighborhood 1 (V2 0 0)
            liftCV $ print (show v)
            yield m
