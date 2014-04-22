{-# LANGUAGE TypeFamilies #-}
module Revelation.Rect (

  Rect (..)
, unsafeCreateRect, createRect
, cloneRect
, topLeft, bottomRight, width, height
, contains

) where

import Revelation.Core
import Revelation.Point
import OpenCV
import Foreign
import Foreign.C
import System.IO.Unsafe
import Control.Applicative

data family Rect e
newtype instance Rect Int32 = MkRectI { extractRectI :: Ptr C'Rect }

unsafeCreateRect :: Rect Int32
unsafeCreateRect = MkRectI . unsafePerformIO $ c'cv_create_Rect

createRect :: Integral a => a -> a -> a -> a -> Rect Int32
createRect x y w h = MkRectI . unsafePerformIO $ c'cv_create_Rect4 (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

cloneRect :: Rect Int32 -> CV (Rect Int32)
cloneRect r = CV $ MkRectI <$> c'cv_Rect_clone (extractRectI r)

genAccessor o f = o . unsafePerformIO . f . extractRectI

topLeft, bottomRight :: Rect Int32 -> Point Two Int32
topLeft = genAccessor MkPoint2i c'cv_Rect_tl
bottomRight = genAccessor MkPoint2i c'cv_Rect_br

width, height :: Integral a => Rect Int32 -> a
width = genAccessor fromIntegral c'cv_Rect_getWidth
height = genAccessor fromIntegral c'cv_Rect_getHeight

contains :: Integral a => Rect Int32 -> Point Two Int32 -> a
contains r p = fromIntegral . unsafePerformIO $ c'cv_Rect_contains (extractRectI r) (extractPt p)
