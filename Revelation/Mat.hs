{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Revelation.Mat ( 
  Channel(..)
, Dimension(..)
, Mat (extract)
, createMat
, rows
, cols
--, index
, dataPtr
--, toVector
) where

import Revelation.Core
import OpenCVRaw.Types
import OpenCVRaw.Mat
import Data.Vector.Storable
import Foreign
import Data.Singletons
import Data.Proxy
import qualified Data.Foldable as F
import Linear (V2(..), V3(..))

data Channel = RGB | BGR | Grayscale | HSV | YUV
data Dimension = TwoD | ThreeD


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

rowPtr :: Storable e => Mat d c e -> Int -> CV (Ptr e)
rowPtr m i = CV $ do
                p <- c'cv_Mat_ptr_index (extract m) (fromIntegral i)
                return $ castPtr p
                      
type family ScalarT (c :: Channel) e :: *
type instance ScalarT Grayscale e = e
type instance ScalarT RGB e = (e, e, e)
type instance ScalarT BGR e = (e, e, e)
type instance ScalarT HSV e = (e, e, e)
type instance ScalarT YUV e = (e, e, e)

type family DimArg (d :: Dimension) :: * -> *
type instance DimArg TwoD = V2
type instance DimArg ThreeD = V3

genSingletons [''Channel]

numChannels' :: Channel -> Int
numChannels' Grayscale = 1
numChannels' _ = 3

numChannels :: forall c. SingI c => Proxy (c :: Channel) -> Int
numChannels _ = numChannels' . fromSing $ (sing :: Sing c)

rawArrayLookup :: Storable e => Mat d c e -> Int -> CV (ScalarT c e)
rawArrayLookup = undefined

index :: forall d c e. (SingI c, F.Foldable (DimArg d)) => Mat d c e -> DimArg d Int -> CV (ScalarT c e)
index m i = rawArrayLookup m (F.product i * nc)
              where nc = numChannels (Proxy :: Proxy c)

toVector m = CV $ do
               p <- runCV $ rowPtr m 0
               p' <- newForeignPtr_ p
               len <- c'cv_Mat_total (extract m)
               return $ unsafeFromForeignPtr0 p' (fromIntegral len)


