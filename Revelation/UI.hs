module Revelation.UI where

import Revelation.Core
import OpenCVRaw.Funcs
import Revelation.Mat
import Foreign.C
import Control.Monad
import Pipes

waitKeyP :: Maybe Char -> Int -> Pipe (Mat d c e) (Mat d c e) CV ()
waitKeyP Nothing n =  forever $ do  
                          mat <- await
                          liftCV $ c'cv_waitKey (fromIntegral n)
                          yield mat
waitKeyP (Just c) n = do  mat <- await
                          cchar <- liftCV $ c'cv_waitKey (fromIntegral n)
                          yield mat
                          when (castCCharToChar (fromIntegral cchar) /= c) $ waitKeyP (Just c) n
