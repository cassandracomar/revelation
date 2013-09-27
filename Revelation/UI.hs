module Revelation.UI where

import Revelation.Bindings.Utils
import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawFuncs
import Revelation.Mat
import Foreign.C
import Control.Monad
import Pipes

waitKey :: Maybe Char -> Int -> Pipe (Mat d c e) (Mat d c e) CV ()
waitKey Nothing n =  do mat <- await
                        liftCV $ c'cv_waitKey (fromIntegral n)
                        yield mat
                        waitKey Nothing n
waitKey (Just c) n = do mat <- await
                        cchar <- liftCV $ c'cv_waitKey (fromIntegral n)
                        when (castCCharToChar (fromIntegral cchar) /= c) $ do 
                          yield mat
                          waitKey (Just c) n
