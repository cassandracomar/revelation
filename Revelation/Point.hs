{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
 
module Revelation.Point (

  Point(..)
, Dim(..)
, PElemT
, ExtractPoint(..)

) where

import Revelation.Core
import Revelation.Mat
import OpenCV
import Foreign
import Foreign.C
import Linear
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative

data Dim = Two | Three
data family Point (s :: Dim) e
data instance Point Two Int32 = MkPoint2i { extractPt2i :: Ptr C'Point }
data instance Point Two Float = MkPoint2f { extractPt2f :: Ptr C'Point2f }
data instance Point Two Double = MkPoint2d { extractPt2d :: Ptr C'Point2d }
data instance Point Three Int32 = MkPoint3i { extractPt3i :: Ptr C'Point3i }
data instance Point Three Float = MkPoint3f { extractPt3f :: Ptr C'Point3f }
data instance Point Three Double = MkPoint3d { extractPt3d :: Ptr C'Point3d }

class ExtractPoint p p' | p -> p' where
  extractPt :: p -> p'

instance ExtractPoint (Point Two Int32) (Ptr C'Point) where
  extractPt = extractPt2i

instance ExtractPoint (Point Three Int32) (Ptr C'Point3i) where
  extractPt = extractPt3i

instance ExtractPoint (Point Two Float) (Ptr C'Point2f) where
  extractPt = extractPt2f

instance ExtractPoint (Point Three Float) (Ptr C'Point3f) where
  extractPt = extractPt3f

instance ExtractPoint (Point Two Double) (Ptr C'Point2d) where
  extractPt = extractPt2d

instance ExtractPoint (Point Three Double) (Ptr C'Point3d) where
  extractPt = extractPt3d

type family PElemT (s :: Dim) :: * -> * where
  PElemT Two = V2
  PElemT Three = V3

class Point2 (s :: Dim) e where
  createPoint :: PElemT s e -> Point s e
  getX :: Point s e -> e
  getY :: Point s e -> e
  dot :: Point s e -> Point s e -> e
  getV :: Point s e -> PElemT s e

class Point3 e where
  getZ :: Point Three e -> e
  cross :: Point Three e -> Point Three e -> Point Three e

instance Point2 Two Int32 where
  createPoint (V2 x y) = MkPoint2i . unsafePerformIO $ c'cv_create_Point2i (fromIntegral x) (fromIntegral y)
  getX = fromIntegral . unsafePerformIO . c'cv_Point2i_getX . extractPt2i
  getY = fromIntegral . unsafePerformIO . c'cv_Point2i_getY . extractPt2i
  dot p1 p2 = fromIntegral . unsafePerformIO $ c'cv_Point2i_dot (extractPt2i p1) (extractPt2i p2)
  getV = (V2 . getX) <*> getY

instance Point3 Int32 where
  getZ = fromIntegral . unsafePerformIO . c'cv_Point3i_getZ . extractPt3i
  cross p1 p2 = MkPoint3i . unsafePerformIO $ c'cv_Point3i_cross (extractPt3i p1) (extractPt3i p2)

instance Point2 Three Int32 where
  createPoint (V3 x y z) = MkPoint3i . unsafePerformIO $ c'cv_create_Point3i (fromIntegral x) 
                                                                             (fromIntegral y) 
                                                                             (fromIntegral z)
  getX = fromIntegral . unsafePerformIO . c'cv_Point3i_getX . extractPt3i
  getY = fromIntegral . unsafePerformIO . c'cv_Point3i_getY . extractPt3i
  dot p1 p2 = fromIntegral . unsafePerformIO $ c'cv_Point3i_dot (extractPt3i p1) (extractPt3i p2)
  getV = (V3 . getX) <*> getY <*> getZ

instance Point2 Two Float where
  createPoint (V2 x y) = MkPoint2f . unsafePerformIO $ c'cv_create_Point2f (realToFrac x) (realToFrac y)
  getX = realToFrac . unsafePerformIO . c'cv_Point2f_getX . extractPt2f
  getY = realToFrac . unsafePerformIO . c'cv_Point2f_getY . extractPt2f
  dot p1 p2 = realToFrac . unsafePerformIO $ c'cv_Point2f_dot (extractPt2f p1) (extractPt2f p2)
  getV = (V2 . getX) <*> getY

instance Point3 Float where
  getZ = realToFrac . unsafePerformIO . c'cv_Point3f_getZ . extractPt3f
  cross p1 p2 = MkPoint3f . unsafePerformIO $ c'cv_Point3f_cross (extractPt3f p1) (extractPt3f p2)

instance Point2 Three Float where
  createPoint (V3 x y z) = MkPoint3f . unsafePerformIO $ c'cv_create_Point3f (realToFrac x) 
                                                                             (realToFrac y) 
                                                                             (realToFrac z)
  getX = realToFrac . unsafePerformIO . c'cv_Point3f_getX . extractPt3f
  getY = realToFrac . unsafePerformIO . c'cv_Point3f_getY . extractPt3f
  dot p1 p2 = realToFrac . unsafePerformIO $ c'cv_Point3f_dot (extractPt3f p1) (extractPt3f p2)
  getV = (V3 . getX) <*> getY <*> getZ

instance Point2 Two Double where
  createPoint (V2 x y) = MkPoint2d . unsafePerformIO $ c'cv_create_Point2d (realToFrac x) (realToFrac y)
  getX = realToFrac . unsafePerformIO . c'cv_Point2d_getX . extractPt2d
  getY = realToFrac . unsafePerformIO . c'cv_Point2d_getY . extractPt2d
  dot p1 p2 = realToFrac . unsafePerformIO $ c'cv_Point2d_dot (extractPt2d p1) (extractPt2d p2)
  getV = (V2 . getX) <*> getY

instance Point3 Double where
  getZ = realToFrac . unsafePerformIO . c'cv_Point3d_getZ . extractPt3d
  cross p1 p2 = MkPoint3d . unsafePerformIO $ c'cv_Point3d_cross (extractPt3d p1) (extractPt3d p2)

instance Point2 Three Double where
  createPoint (V3 x y z) = MkPoint3d . unsafePerformIO $ c'cv_create_Point3d (realToFrac x) 
                                                                             (realToFrac y) 
                                                                             (realToFrac z)
  getX = realToFrac . unsafePerformIO . c'cv_Point3d_getX . extractPt3d
  getY = realToFrac . unsafePerformIO . c'cv_Point3d_getY . extractPt3d
  dot p1 p2 = realToFrac . unsafePerformIO $ c'cv_Point3d_dot (extractPt3d p1) (extractPt3d p2)
  getV = (V3 . getX) <*> getY <*> getZ
