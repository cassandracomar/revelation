module Revelation.Video (
  cameraCapture
, VideoCapture
, Window
, imageDisplayWindow
)where

import OpenCV
import Development.CPPUtils
import Revelation.Mat
import Revelation.Core
import Foreign.Ptr

import Control.Monad
import Pipes

type VideoCapture c e = Producer (Mat c e) CV ()

cameraCapture :: Int -> VideoCapture c e
cameraCapture d = liftCV (c'cv_create_VideoCapture1_0 $ fromIntegral d) >>= _capture
                     
_capture :: Ptr C'VideoCapture -> VideoCapture c e
_capture cap = lift (return unsafeCreateMat) >>= loop cap
                    where
                      loop c mat = do
                        liftCV $ c'cv_VideoCapture_read c (extract mat)
                        yield mat
                        loop cap mat

type Window c e = Consumer (Mat c e) CV ()

imageDisplayWindow :: String -> Window c e
imageDisplayWindow name = do cname <- liftCV $ toStdString name
                             liftCV $ c'cv_namedWindow (castPtr cname) c'CV_WINDOW_NORMAL0
                             forever $ do
                               mat <- await
                               liftCV $ c'cv_imshow (castPtr cname) (extract mat)
