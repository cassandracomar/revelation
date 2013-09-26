module Revelation.Video where

import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawConsts
import Revelation.Bindings.RawFuncs
import Revelation.Bindings.Mat
import Revelation.Bindings.CppTypes
import Foreign.Ptr

import Control.Monad
import Pipes

type VideoCapture = Producer Mat IO ()

cameraCapture :: Int -> VideoCapture
cameraCapture d = lift (c'cv_create_VideoCapture1 $ fromIntegral d) >>= _capture
                     
_capture :: Ptr C'VideoCapture -> VideoCapture
_capture cap = liftIO c'cv_create_Mat >>= loop cap
                    where
                      loop cap mat = do
                        lift $ c'cv_VideoCapture_read cap mat
                        yield mat
                        loop cap mat

type Window = Consumer Mat IO ()

imageDisplayWindow :: String -> Window
imageDisplayWindow name = do cname <- lift $ toStdString name
                             lift $ c'cv_namedWindow cname c'CV_WINDOW_NORMAL0
                             forever $ do
                               mat <- await
                               lift $ c'cv_imshow cname mat
