module Revelation.Video (
  cameraCapture
, VideoCapture
, Window
, imageDisplayWindow
)where

import OpenCVRaw.Types
import OpenCVRaw.Consts
import OpenCVRaw.Funcs
import Revelation.Mat
import OpenCVRaw.CppTypes
import Revelation.Core
import Foreign.Ptr

import Control.Monad
import Pipes

type VideoCapture d c e = Producer (Mat d c e) CV ()

cameraCapture :: Int -> VideoCapture d c e
cameraCapture d = liftCV (c'cv_create_VideoCapture1 $ fromIntegral d) >>= _capture
                     
_capture :: Ptr C'VideoCapture -> VideoCapture d c e
_capture cap = lift createMat >>= loop cap
                    where
                      loop c mat = do
                        liftCV $ c'cv_VideoCapture_read c (extract mat)
                        yield mat
                        loop cap mat

type Window d c e = Consumer (Mat d c e) CV ()

imageDisplayWindow :: String -> Window d c e
imageDisplayWindow name = do cname <- liftCV $ toStdString name
                             liftCV $ c'cv_namedWindow cname c'CV_WINDOW_NORMAL0
                             forever $ do
                               mat <- await
                               liftCV $ c'cv_imshow cname (extract mat)
