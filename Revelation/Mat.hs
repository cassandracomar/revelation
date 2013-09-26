{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Revelation.Mat ( 
  Channels(..)
, Dimensions(..)
, Mat (extract)
, createMat
, unsafeMatToVector
, unsafeIndexMat
, module Revelation.Bindings.Mat
) where

import Revelation.Bindings.RawTypes
import Revelation.Bindings.Mat
import Data.Vector.Storable
import Foreign.ForeignPtr
import Foreign.Ptr

data Channels = RGB | BGR | Grayscale
data Dimensions = TwoD | ThreeD

newtype Mat (d :: Dimensions) (c :: Channels) elem = MkMat { extract :: Ptr C'Mat }

createMat :: IO (Mat (d :: Dimensions) (c :: Channels) e)
createMat = do m <- c'cv_create_Mat
               return $ MkMat m

unsafeMatToVector :: Storable e => Mat d c e -> IO (Vector e)
unsafeMatToVector m = do  p <- c'cv_Mat_ptr (extract m)
                          p' <- newForeignPtr_ p
                          len <- c'cv_Mat_total (extract m)
                          return $ unsafeFromForeignPtr0 (castForeignPtr p') (fromIntegral len)

unsafeIndexMat :: Storable e => Mat d c e -> Int -> IO e
unsafeIndexMat m ix = do v <- unsafeMatToVector m 
                         return $ v ! ix
