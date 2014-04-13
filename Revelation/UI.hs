module Revelation.UI where

import Revelation.Core
import OpenCV
import Revelation.Mat
import Foreign.C
import Control.Monad
import Pipes

waitKey :: Integral a => a -> CV Char
waitKey n = CV $ do c <- c'cv_waitKey (fromIntegral n)
                    return $ castCCharToChar (fromIntegral c)

waitKeyP :: Maybe Char -> Int -> Pipe (Mat m n c e) (Mat m n c e) CV ()
waitKeyP Nothing n =  forever $ do  
                          mat <- await
                          lift $ waitKey n
                          yield mat
waitKeyP (Just c) n = do  mat <- await
                          c' <- lift $ waitKey n
                          yield mat
                          when (c' /= c) $ waitKeyP (Just c) n
