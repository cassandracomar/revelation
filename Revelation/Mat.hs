{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Revelation.Mat ( 
-- ** Types
  Channel(..)
, Mat (extract)
, MatExpr (extractExpr)
, ElemT
, CVElement(..)
-- ** Constructors
, createMat, unsafeCreateMat
, createIdentity, ones, zeros
-- ** Functions
, rows, cols
, subMat
, getAt, setAt
, fromMat, toMat, asSingleVector
, pixel, neighborhood
, promote, force
, promoting, forcing
, InverseMethod(..), invert, invertBy
, transpose
, (.+.), (.*.), (.*), (*.)
) where

import Prelude hiding ((!!))
import Revelation.Core
import OpenCV
import Foreign
import Foreign.C
import Linear 
import qualified Data.Vector.Storable as VS
import Control.Lens
import Control.Applicative
import Control.Monad

-- | Promoted data type to track channel type at the type level.
data Channel = RGB | BGR | Grayscale | HSV | YUV

-- | Matrix type - type parameter c must have kind Channel.
-- Use extract to get at the underlying C'Mat ptr.
newtype Mat (c :: Channel) elem = MkMat { extract :: Ptr C'Mat }

-- | Lazily evaluated matrix expressions. OpenCV uses this type to perform
-- some optimizations on matrix operations, so it's preserved. Usage of
-- this type is generally referentially transparent because there's no way
-- to inspect its value, and it's used only for mathematical operations.
-- However, expressions are **unchecked**! That means you'll get a runtime
-- error if you attempt to add or multiply matrices of the wrong type.
-- This would be tracked in the types, but it's currently very difficult to
-- do.
newtype MatExpr (c :: Channel) elem = MkMatExpr { extractExpr :: Ptr C'MatExpr }

-- | Type synonym family to deal with the storage format for matrices
-- of various channels. If you need to pass something of this type
-- to a constructor, it's always safe to pass (undefined :: ElemT c e)
-- with c and e filled in as appropriate.
type family ElemT (c :: Channel) :: * -> *
type instance ElemT Grayscale = V1
type instance ElemT RGB = V3 
type instance ElemT BGR = V3
type instance ElemT HSV = V3
type instance ElemT YUV = V3

-- | Typeclass to convert the provided element type to an OpenCV element
-- type.
class CVElement f where
  cvElemType :: f -> CInt

instance CVElement (V1 Double) where
  cvElemType _ = c'CV_64FC1

instance CVElement (V1 Float) where
  cvElemType _ = c'CV_32FC1

-- Int has a specified size, but it's not statically guaranteed
-- So instances are provided for the guaranteed fixed width types
-- Use fromIntegral to the correct width type.
instance CVElement (V1 Int32) where
  cvElemType _ = c'CV_32SC1

instance CVElement (V1 Int16) where
  cvElemType _ = c'CV_16SC1

instance CVElement (V1 Int8) where
  cvElemType _ = c'CV_8SC1

instance CVElement (V3 Double) where
  cvElemType _ = c'CV_64FC3

instance CVElement (V3 Float) where
  cvElemType _ = c'CV_32FC3

instance CVElement (V3 Int32) where
  cvElemType _ = c'CV_32SC3

instance CVElement (V3 Int16) where
  cvElemType _ = c'CV_16SC3

instance CVElement (V3 Int8) where
  cvElemType _ = c'CV_8SC3

-- | A safe matrix creation function. This one gives you exactly what you ask
-- for, and guarantees that a matrix of the appropriate type and size is 
-- allocated. The ElemT c e parameter is safely passed as undefined because
-- it's value is never inspected. It's needed as a type witness.
-- The witness can't be created automatically with GHC 7.6 because type
-- families are open and not guaranteed to be injective.
createMat :: CVElement (ElemT c e) => Int -> Int -> ElemT c e -> CV (Mat c e)
createMat rs cs proxy = do m <- CV $ c'cv_create_Mat_typed (fromIntegral rs) (fromIntegral cs) (cvElemType proxy)
                           return $ MkMat m

-- | This return type potentially lies... use only if you're truly
-- polymorphic in the type of the underlying matrix.
unsafeCreateMat :: CV (Mat c e)
unsafeCreateMat = CV $ do 
                m <- c'cv_create_Mat
                return $ MkMat m

-- | Allocates a square identity matrix of size r x r
createIdentity :: CVElement (ElemT c e) => Int -> ElemT c e -> CV (Mat c e)
createIdentity r proxy = CV $ do
                            m <- c'cv_create_identity (fromIntegral r) (fromIntegral r) (cvElemType proxy)
                            return $ MkMat m
   
-- | Allocates a matrix of the requested size and fills it with ones.
ones :: CVElement (ElemT c e) => Int -> Int -> ElemT c e -> CV (Mat c e)
ones rs cs proxy = CV $ do
                     m <- c'cv_create_ones (fromIntegral rs) (fromIntegral cs) (cvElemType proxy)
                     return $ MkMat m

-- | Allocates a matrix of the requested size and fills it with zeroes.
zeros :: CVElement (ElemT c e) => Int -> Int -> ElemT c e -> CV (Mat c e)
zeros rs cs proxy = CV $ do
                     m <- c'cv_create_zeros (fromIntegral rs) (fromIntegral cs) (cvElemType proxy)
                     return $ MkMat m

-- | Extract the number of rows in the provide matrix (equivalent to field
-- accessor rows on the C++ side) 
rows :: Integral a => Mat c e -> CV a
rows m = CV $ do
            r <- c'cv_Mat_rows (extract m)
            return $ fromIntegral r

-- | Extract the number of rows in the provide matrix (equivalent to field
-- accessor cols on the C++ side) 
cols :: Integral a => Mat c e -> CV a
cols m = CV $ do
            c <- c'cv_Mat_cols (extract m)
            return $ fromIntegral c

-- | Extracts a pointer to a given row, and returns a correctly typed
-- C Array. 
rowPtr :: Storable (ElemT c e) => Mat c e -> Int -> CV (Ptr (ElemT c e))
rowPtr m i = CV $ do
                p <- c'cv_Mat_ptr_index (extract m) (fromIntegral i)
                return $ castPtr p

-- | Extracts a sub matrix from a given matrix.
-- | First parameter is top left, second parameter is bottom right.
subMat :: Mat c e -> V2 Int -> V2 Int -> CV (Mat c e)
subMat m (V2 i j) (V2 k l) = CV $ do
                                m' <- c'cv_Mat_getRowRange (extract m) (fromIntegral i) (fromIntegral k)
                                m'' <- c'cv_Mat_getColRange m' (fromIntegral j) (fromIntegral l)
                                return $ MkMat m''

-- | Index into a matrix at (row, column) given by (V2 row column).
-- | Getter
getAt :: Storable (ElemT c e) => V2 Int -> Mat c e -> CV (ElemT c e)
getAt (V2 i j) m = do p <- rowPtr m i
                      CV $ peekElemOff p j

-- | Index into a matrix at (row, column) given by (V2 row column).
-- | Setter
setAt :: Storable (ElemT c e) => V2 Int -> Mat c e -> ElemT c e -> CV (Mat c e)
setAt (V2 i j) m e = do p <- rowPtr m i
                        CV $ pokeElemOff p j e
                        return m

-- | Lens for indexed access into a Mat. The matrix and element are always
-- in CV because getting and setting can't be done safely outside of
-- a monad.
pixel :: Storable (ElemT c e) => V2 Int -> Lens' (CV (Mat c e)) (CV (ElemT c e))
pixel i = lens getter setter
      where getter m = m >>= getAt i
            setter m e = join $ setAt i <$> m <*> e

-- | provides the 8/24/48/etc. neighborhood around a pixel (including the pixel
-- itself). The first parameter is the max pixel distance from 
getNeighborhood :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Int -> V2 Int -> Mat c e -> CV (VS.Vector (VS.Vector (ElemT c e)))
getNeighborhood s (V2 i j) m = do rs <- rows m
                                  cs <- cols m
                                  m' <- subMat m tl (br rs cs)
                                  fromMat m'
                                    where clampedLower k = if k < 0 then 0 else k
                                          clampedHigher b k = if k >= b then b else k
                                          tl = V2 (clampedLower $ i - s) (clampedLower $ j - s)
                                          br rs cs = V2 (clampedHigher rs $ i + s) (clampedHigher cs $ i + s)

-- | Sets the entire neighborhood for a pixel. This is a relatively
-- inefficient function because we can't just write the entire vector at
-- once and instead have to write each element individually.
setNeighborhood :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Int -> V2 Int -> Mat c e -> VS.Vector (VS.Vector (ElemT c e)) -> CV (Mat c e)
setNeighborhood s (V2 i j) m v = do rs <- rows m
                                    cs <- cols m
                                    VS.forM_ (inds rs cs) $ \k -> (return m) & pixel k .~ (return $ v !! k)
                                    return m
                                    where inds rs cs = VS.fromList [V2 x y | x <- [(i - s) .. (i + s)]
                                                                           , y <- [(j - s) .. (j + s)] 
                                                                           , x >= 0 && x < rs
                                                                           , y >= 0 && y < cs]
                                          v' !! (V2 i' j') = v' VS.! i' VS.! j'

-- | Lens to the 8/24/48/etc. neighborhood of a pixel (including the pixel
-- itself).
neighborhood :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Int -> V2 Int -> Lens' (CV (Mat c e)) (CV (VS.Vector (VS.Vector (ElemT c e))))
neighborhood s i = lens getter setter
                    where getter m = m >>= getNeighborhood s i
                          setter m e = join $ setNeighborhood s i <$> m <*> e

-- | Create a Vector (of Vectors)
-- (provided for integration with linear)
fromMat :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Mat c e -> CV (VS.Vector (VS.Vector (ElemT c e)))
fromMat m = CV $ do 
              rs <- runCV $ rows m
              cs <- runCV $ cols m
              VS.forM (VS.fromList [0 .. (rs-1)]) $ \i -> do
                p <- runCV $ rowPtr m i
                p' <- newForeignPtr_ p
                return $ VS.unsafeFromForeignPtr0 p' cs

-- | Turn a Vector (of Vectors) into a Mat
-- This function lets you create a matrix intialized with the values from
-- the Vector of vectors.
toMat :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e)), CVElement (ElemT c e)) => VS.Vector (VS.Vector (ElemT c e)) -> CV (Mat c e)
toMat v = CV $ MkMat <$> VS.unsafeWith v makeMat
              where makeMat p = c'cv_create_Mat_with_data (fromIntegral rs) (fromIntegral cs) (cvElemType e) p 
                    rs = VS.length v
                    cs = VS.length $ v VS.! 0
                    e = v VS.! 0 VS.! 0

-- | Create a single long Vector (lazily)
asSingleVector :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Mat c e -> CV (VS.Vector (ElemT c e))
asSingleVector m = do v <- fromMat m 
                      return $ VS.foldr (VS.++) VS.empty v

-- | The decomposition method used to compute the inverse
data InverseMethod = LU | Cholesky | SVD

inverseMethod :: InverseMethod -> CInt
inverseMethod LU = c'CV_DECOMP_LU0
inverseMethod Cholesky = c'CV_DECOMP_CHOLESKY0
inverseMethod SVD = c'CV_DECOMP_SVD0

-- | Inverts the given matrix. Uses SVD to guarantee that *something* is
-- always returned. The matrix provided is guaranteed to at least be a
-- pseudo-inverse (left or right). Gives a true inverse when the matrix
-- is square and non-singular.
invert :: MatExpr c e -> MatExpr c e
invert = invertBy SVD

-- | Inverts the given matrix with the provided method. This function is
-- partial if the method passed is not SVD *and* a singular or non-square
-- matrix is passed in.
invertBy :: InverseMethod -> MatExpr c e -> MatExpr c e
invertBy im m = MkMatExpr $ c'cv_Mat_inv_mat (extractExpr m) (inverseMethod im)

transpose :: MatExpr c e -> MatExpr c e
transpose = MkMatExpr . c'cv_Mat_transpose_mat . extractExpr

-- | Forces the evaluation of an accumulated Matrix Expression.
force :: MatExpr c e -> CV (Mat c e)
force m = CV $ do
            m' <- c'force (extractExpr m)
            return $ MkMat m'

-- | Promotes a matrix to a matrix expression. Think (\m -> (\() -> m)).
promote :: Mat c e -> MatExpr c e
promote = MkMatExpr . c'promote . extract

-- / Convenience Iso for promotion 
promoting :: Iso (Mat c e) (CV (Mat c e)) (MatExpr c e) (MatExpr c e)
promoting = iso promote force

-- | Convenience Iso for forcing
forcing :: Iso (MatExpr c e) (MatExpr c e) (CV (Mat c e)) (Mat c e)
forcing = from promoting

-- | standard matrix addition
(.+.) :: MatExpr c e -> MatExpr c e -> MatExpr c e
m1 .+. m2 = MkMatExpr $ (extractExpr m1) `c'cv_Mat_add` (extractExpr m2)

-- | Normal matrix multiplication.
(.*.) :: MatExpr c e -> MatExpr c e -> MatExpr c e
m1 .*. m2 = MkMatExpr $ (extractExpr m1) `c'cv_Mat_mult` (extractExpr m2) 

-- | Matrix left scaling
(*.) :: Double -> MatExpr c e -> MatExpr c Double
a *. m = MkMatExpr $ (extractExpr m) `c'cv_Mat_scale` (realToFrac a)

-- | Matrix right scaling
(.*) :: MatExpr c e -> Double -> MatExpr c Double
m .* a = MkMatExpr $ (extractExpr m) `c'cv_Mat_scale` (realToFrac a)
