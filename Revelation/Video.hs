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

type VideoCapture m n c e = Producer (Mat m n c e) CV ()

cameraCapture :: Int -> VideoCapture m n c e
cameraCapture d = liftCV (c'cv_create_VideoCapture1_0 $ fromIntegral d) >>= _capture
                     
_capture :: Ptr C'VideoCapture -> VideoCapture m n c e
_capture cap = lift (return unsafeCreateMat) >>= loop cap
                    where
                      loop c mat = do
                        liftCV $ c'cv_VideoCapture_read c (extract mat)
                        yield mat
                        loop cap mat

type Window m n c e = Consumer (Mat m n c e) CV ()

imageDisplayWindow :: String -> Window m n c e
imageDisplayWindow name = do cname <- liftCV $ toStdString name
                             liftCV $ c'cv_namedWindow (castPtr cname) c'CV_WINDOW_NORMAL0
                             forever $ do
                               mat <- await
                               liftCV $ c'cv_imshow (castPtr cname) (extract mat)
