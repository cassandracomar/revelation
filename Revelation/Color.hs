{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Revelation.Color (
  Channel(..)
, convertColorP
, convertColor
) where

import Revelation.Core
import OpenCV
import Revelation.Mat

import Pipes
import Control.Monad

class Convertable (a :: Channel) (b :: Channel) where
  cvtValue :: Num c => Mat m n a e -> Mat m n b e -> c

instance Convertable RGB BGR where
  cvtValue _ _ = c'CV_COLOR_RGB2BGR0

instance Convertable RGB Grayscale where
  cvtValue _ _ = c'CV_COLOR_RGB2GRAY0

instance Convertable BGR Grayscale where
  cvtValue _ _ = c'CV_COLOR_BGR2GRAY0

instance Convertable BGR RGB where
  cvtValue _ _ = c'CV_COLOR_BGR2RGB0

instance Convertable BGR YUV where
  cvtValue _ _ = c'CV_COLOR_BGR2YUV0

instance Convertable YUV RGB where
  cvtValue _ _ = c'CV_COLOR_YUV2RGB0

instance Convertable RGB YUV where
  cvtValue _ _ = c'CV_COLOR_RGB2YUV0

instance Convertable YUV BGR where
  cvtValue _ _ = c'CV_COLOR_YUV2BGR0

convertColorP :: Convertable c c' => Pipe (Mat m n c e) (Mat m n c' e) CV ()
convertColorP = forever $ do 
                  m  <- await
                  m' <- lift $ convertColor m
                  yield m'

convertColor :: Convertable c c' => Mat m n c e -> CV (Mat m n c' e)
convertColor m = do let m' = unsafeCreateMat
                    m'' <- clone m'
                    CV $ c'cv_cvtColor (extract m) (extract m'') (cvtValue m m'') 0
                    return m''
