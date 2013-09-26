module Revelation.Video where

import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawConsts
import Revelation.Bindings.RawFuncs
import Revelation.Mat
import Revelation.Bindings.CppTypes
import Foreign.Ptr

import Control.Monad
import Pipes

type VideoCapture d c e = Producer (Mat d c e) IO ()

cameraCapture :: Int -> VideoCapture d c e
cameraCapture d = lift (c'cv_create_VideoCapture1 $ fromIntegral d) >>= _capture
                     
_capture :: Ptr C'VideoCapture -> VideoCapture d c e
_capture cap = liftIO createMat >>= loop cap
                    where
                      loop cap mat = do
                        lift $ c'cv_VideoCapture_read cap (extract mat)
                        yield mat
                        loop cap mat

type Window d c e = Consumer (Mat d c e) IO ()

imageDisplayWindow :: String -> Window d c e
imageDisplayWindow name = do cname <- lift $ toStdString name
                             lift $ c'cv_namedWindow cname c'CV_WINDOW_NORMAL0
                             forever $ do
                               mat <- await
                               lift $ c'cv_imshow cname (extract mat)
