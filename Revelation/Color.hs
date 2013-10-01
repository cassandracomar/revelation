{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Revelation.Color (
  Channel(..)
, convertColorP
, convertColor
) where

import Revelation.Core
import OpenCVRaw.Consts
import OpenCVRaw.Funcs
import Revelation.Mat

import Pipes
import Control.Monad

class Convertable (a :: Channel) (b :: Channel) where
  cvtValue :: Num c => Mat a e -> Mat b e -> c

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

convertColorP :: Convertable c c' => Pipe (Mat c e) (Mat c' e) CV ()
convertColorP = forever $ do 
                  m  <- await
                  m' <- lift $ convertColor m
                  yield m'

convertColor :: Convertable c c' => Mat c e -> CV (Mat c' e)
convertColor m = do m' <- unsafeCreateMat
                    CV $ c'cv_cvtColor (extract m) (extract m') (cvtValue m m') 0
                    return m'
