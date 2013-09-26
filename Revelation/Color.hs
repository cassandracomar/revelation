{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Revelation.Color (
  Channel(..)
, convertColor
) where

import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawConsts
import Revelation.Bindings.RawFuncs
import Revelation.Mat
import Revelation.Bindings.CppTypes
import Foreign.Ptr

import Pipes
import Control.Monad

class Convertable (a :: Channel) (b :: Channel) where
  cvtValue :: Num c => Mat d a e -> Mat d b e -> c

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

instance Convertable YUV Grayscale where
  cvtValue _ _ = c'CV_COLOR_YUV2Grayscale0

convertColor :: Convertable c c' => Pipe (Mat d c e) (Mat d c' e) IO ()
convertColor = forever $ do 
                  m  <- await
                  m' <- lift $ createMat
                  lift $ c'cv_cvtColor (extract m) (extract m') (cvtValue m m') 0
                  yield m'

