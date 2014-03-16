{-# LANGUAGE FlexibleContexts #-}
module Revelation.Utils where
import Pipes
import Linear
import Revelation.Mat
import Revelation.Core
import Control.Monad
import Control.Lens
import Data.Vector.Storable as VS

indexP :: (Storable (ElemT c e), Show (ElemT c e)) => V2 Int -> Pipe (Mat c e) (Mat c e) CV ()
indexP i = forever $ do 
              m <- await
              v <- lift $ return m ^. pixel i
              liftCV $ print (show v)
              yield m
