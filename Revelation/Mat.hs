{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Revelation.Mat ( 
  Channel(..)
, Dimension(..)
, Mat (extract)
, createMat
, rows
, cols
, index
, fromMat
) where

import Revelation.Core
import OpenCVRaw.Types
import OpenCVRaw.Mat
import Foreign
import Linear 
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

data Channel = RGB | BGR | Grayscale | HSV | YUV
data Dimension = TwoD | ThreeD


newtype Mat (c :: Channel) elem = MkMat { extract :: Ptr C'Mat }

createMat :: CV (Mat c e)
createMat = CV $ do 
                m <- c'cv_create_Mat
                return $ MkMat m

rows :: Integral a => Mat c e -> CV a
rows m = CV $ do
            r <- c'cv_Mat_rows (extract m)
            return $ fromIntegral r

cols :: Integral a => Mat c e -> CV a
cols m = CV $ do
            c <- c'cv_Mat_cols (extract m)
            return $ fromIntegral c

type family ElemT (c :: Channel) :: * -> *
type instance ElemT Grayscale = V1
type instance ElemT RGB = V3 
type instance ElemT BGR = V3
type instance ElemT HSV = V3
type instance ElemT YUV = V3

rowPtr :: Storable (ElemT c e) => Mat c e -> Int -> CV (Ptr (ElemT c e))
rowPtr m i = CV $ do
                p <- c'cv_Mat_ptr_index (extract m) (fromIntegral i)
                return $ castPtr p
                      
index :: Storable (ElemT c e) => Mat c e -> V2 Int -> CV (ElemT c e)
index m (V2 i j) = do p <- rowPtr m i
                      CV $ peekElemOff p j

fromMat :: Storable (ElemT c e) => Mat c e -> CV (V.Vector (VS.Vector (ElemT c e)))
fromMat m = CV $ do 
              rs <- runCV $ rows m
              cs <- runCV $ cols m
              V.forM (V.fromList [0 .. (rs-1)]) $ \i -> do
                p <- runCV $ rowPtr m i
                p' <- newForeignPtr_ p
                return $ VS.unsafeFromForeignPtr0 p' cs
