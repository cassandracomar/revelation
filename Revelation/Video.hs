module Revelation.Video (
  cameraCapture
  , fileCapture
  , VideoCapture
  , Window
  , imageDisplayWindow
)where

import OpenCVRaw.Types
import OpenCVRaw.Consts
import OpenCVRaw.Funcs
import Revelation.Mat
import OpenCVRaw.CppTypes
import OpenCVRaw.CapFile
import Revelation.Core
import Foreign.Ptr
import Foreign.C.String

import Control.Monad
import Pipes

type VideoCapture c e = Producer (Mat c e) CV ()

cameraCapture :: Int -> VideoCapture c e
cameraCapture d = liftCV (c'cv_create_VideoCapture1 $ fromIntegral d) >>= _capture

fileCapture :: String -> VideoCapture c e
fileCapture fn = liftCV (withCString fn c'cv_create_VideoCapture_file) >>= _capture

_capture :: Ptr C'VideoCapture -> VideoCapture c e
_capture cap = lift unsafeCreateMat >>= loop cap
                    where
                      loop c mat = do
                        liftCV $ c'cv_VideoCapture_read c (extract mat)
                        yield mat
                        loop cap mat

type Window c e = Consumer (Mat c e) CV ()

imageDisplayWindow :: String -> Window c e
imageDisplayWindow name = do cname <- liftCV $ toStdString name
                             liftCV $ c'cv_namedWindow cname c'CV_WINDOW_NORMAL0
                             forever $ do
                               mat <- await
                               liftCV $ c'cv_imshow cname (extract mat)
