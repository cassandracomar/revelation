module Revelation.Utils where
import Pipes
import Linear
import Revelation.Mat
import Revelation.Core
import Control.Monad

indexP :: Pipe (Mat Grayscale Int) (Mat Grayscale Int) CV ()
indexP = forever $ do 
            m <- await
            v <- lift $ index m (V2 0 0)
            liftCV $ print v
            yield m

