{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Revelation.Mat ( 
  Channel(..)
, Dimension(..)
, Mat (extract)
, createMat
, rows
, cols
, matToVector
, index2_1
, index2_3
) where

import Revelation.Core
import OpenCVRaw.Types
import OpenCVRaw.Mat
import Data.Vector.Storable
import Foreign
import GHC.Prim

data Channel = RGB | BGR | Grayscale | HSV | YUV
data Dimension = TwoD | ThreeD

type family SingleChannel (a :: Channel) :: Constraint
type instance SingleChannel a = a ~ Grayscale

-- This is ugly, but the only way outside of HEAD to provide the correct
-- constraint.
type family ThreeChannel (a :: Channel) :: Constraint
type instance ThreeChannel Grayscale = Grayscale ~ RGB
type instance ThreeChannel RGB = RGB ~ RGB
type instance ThreeChannel BGR = BGR ~ BGR
type instance ThreeChannel HSV = HSV ~ HSV
type instance ThreeChannel YUV = YUV ~ YUV

newtype Mat (d :: Dimension) (c :: Channel) elem = MkMat { extract :: Ptr C'Mat }

createMat :: CV (Mat d c e)
createMat = CV $ do 
                m <- c'cv_create_Mat
                return $ MkMat m

rows :: Integral a => Mat d c e -> CV a
rows m = CV $ do
            r <- c'cv_Mat_rows (extract m)
            return $ fromIntegral r

cols :: Integral a => Mat d c e -> CV a
cols m = CV $ do
            c <- c'cv_Mat_cols (extract m)
            return $ fromIntegral c

dataPtr :: Storable e => Mat d c e -> CV (Ptr e)
dataPtr m = CV $ do 
                p <- c'cv_Mat_ptr (extract m)
                return $ castPtr p

                      
matToVector :: Storable e => Mat d c e -> CV (Vector e)
matToVector m = CV $ do  
                    p <- c'cv_Mat_ptr (extract m)
                    p' <- newForeignPtr_ p
                    len <- c'cv_Mat_total (extract m)
                    return $ unsafeFromForeignPtr0 (castForeignPtr p') (fromIntegral len)

-- Once GHC HEAD (7.8) is released, we can provide a single index function that
-- correctly takes the right arguments and returns the right elements.
index2_1 :: (Storable e, SingleChannel c) => Mat TwoD c e -> Int -> Int -> CV e
index2_1 m i j = CV $ do 
                      p <- runCV $ dataPtr m
                      c <- runCV $ cols m
                      peekElemOff p $ i * c + j

index2_3 :: (Storable e, ThreeChannel c) => Mat TwoD c e -> Int -> Int -> CV (e, e, e)
index2_3 m i j = CV $ do
                      p <- runCV $ dataPtr m
                      c <- runCV $ cols m
                      e1 <- peekElemOff p $ (i * c + j)
                      e2 <- peekElemOff p $ (i * c + j + 1)
                      e3 <- peekElemOff p $ (i * c + j + 2)
                      return (e1, e2, e3)
