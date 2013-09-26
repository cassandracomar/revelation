{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Revelation.Mat ( 
  Channel(..)
, Dimension(..)
, Mat (extract)
, createMat
, matToVector
, indexMat
, module Revelation.Bindings.Mat
) where

import Revelation.Bindings.RawTypes
import Revelation.Bindings.Mat
import Data.Vector.Storable
import Foreign.ForeignPtr
import Foreign.Ptr

data Channel = RGB | BGR | Grayscale | HSV | YUV
data Dimension = TwoD | ThreeD

newtype Mat (d :: Dimension) (c :: Channel) elem = MkMat { extract :: Ptr C'Mat }

createMat :: IO (Mat d c e)
createMat = do m <- c'cv_create_Mat
               return $ MkMat m

matToVector :: Storable e => Mat d c e -> IO (Vector e)
matToVector m = do  p <- c'cv_Mat_ptr (extract m)
                    p' <- newForeignPtr_ p
                    len <- c'cv_Mat_total (extract m)
                    return $ unsafeFromForeignPtr0 (castForeignPtr p') (fromIntegral len)

indexMat :: Storable e => Mat d c e -> Int -> IO e
indexMat m i = do v <- matToVector m 
                  return $ v ! i
