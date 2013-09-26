{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
module Revelation.UI where

import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawFuncs
import Revelation.Mat
import Foreign.C
import Control.Monad
import Pipes

waitKey :: forall (d :: Dimensions) (c :: Channels) e. Maybe Char -> Int -> Pipe (Mat d c e) (Mat d c e) IO ()
waitKey Nothing n =  do mat <- await
                        lift $ c'cv_waitKey (fromIntegral n)
                        yield mat
                        waitKey Nothing n
waitKey (Just c) n = do mat <- await
                        cchar <- lift $ c'cv_waitKey (fromIntegral n)
                        when (castCCharToChar (fromIntegral cchar) /= c) $ do 
                          yield mat
                          waitKey (Just c) n
