{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

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
, clone
-- ** Functions
, rows, cols
, subMat
, getAt, setAt
, fromMat, toMat, asVector, asSingleVector
, pixel, neighborhood
, promote, force, promoting
, InverseMethod(..), invert, invertBy
, transpose
, (.+.), (.*.), (.*), (*.)
) where

import Revelation.Core
import OpenCV
import Foreign
import Foreign.C
import Linear 
import qualified Data.Vector.Storable as VS
import Control.Lens
import qualified System.IO.Unsafe as S (unsafePerformIO)
import Control.Applicative

-- | Promoted data type to track channel type at the type level.
data Channel = RGB | BGR | Grayscale | HSV | YUV

-- | Matrix type - type parameter c must have kind Channel.
newtype Mat (c :: Channel) elem = MkMat { extract :: Ptr C'Mat }

-- | Lazily evaluated matrix expressions. OpenCV uses this type to perform
-- | some optimizations on matrix operations, so it's preserved. Usage of
-- | this type is generally referentially transparent because there's no way
-- | to inspect its value, and it's used only for mathematical operations.
-- | However, expressions are **unchecked**! That means you'll get a runtime
-- | error if you attempt to add or multiply matrices of the wrong type.
-- | This would be tracked in the types, but it's currently very difficult to
-- | do. Once GHC 7.8 is released, I'll update this.
newtype MatExpr (c :: Channel) elem = MkMatExpr { extractExpr :: Ptr C'MatExpr }

-- | Type synonym family to deal with the storage format for matrices
-- of various channels. If you need to pass something of this type
-- to a constructor, it's always safe to pass (undefined :: ElemT c e)
-- with c and e filled in as appropriate.
type family ElemT (c :: Channel) :: * -> * where
  ElemT Grayscale = V1
  ElemT RGB = V3 
  ElemT BGR = V3
  ElemT HSV = V3
  ElemT YUV = V3

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

instance CVElement (V1 Word8) where
  cvElemType _ = c'CV_8UC1

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

instance CVElement (V3 Word8) where
  cvElemType _ = c'CV_8UC3

-- | A safe matrix creation function. This one gives you exactly what you ask
-- | for, and guarantees that a matrix of the appropriate type and size is 
-- | allocated. The ElemT c e parameter is safely passed as undefined because
-- | it's value is never inspected. It's needed as a type witness.
-- | The witness can't be created automatically with GHC 7.6 because type
-- | families are open and not guaranteed to be injective.
createMat :: CVElement (ElemT c e) => Int -> Int -> ElemT c e -> Mat c e
createMat rs cs proxy = MkMat $ c'cv_create_Mat_typed (fromIntegral rs) (fromIntegral cs) (cvElemType proxy)

-- | This return type potentially lies... use only if you're truly
-- | polymorphic in the type of the underlying matrix.
-- | i.e. if you're wanting any old mat to pass to an opencv function
-- | that will correctly intialize it for you.
unsafeCreateMat :: Mat c e
unsafeCreateMat = MkMat $ c'cv_create_Mat 

-- | Clones the matrix, copying the underlying data.
-- | This does a deep copy.
clone :: Mat c e -> CV (Mat c e)
clone m = CV $ MkMat <$> c'cv_Mat_clone (extract m)

-- | Allocates a square identity matrix of size r x r
createIdentity :: CVElement (ElemT c e) => Int -> ElemT c e -> Mat c e
createIdentity r proxy = MkMat $ c'cv_create_identity (fromIntegral r) (fromIntegral r) (cvElemType proxy)
   
-- | Allocates a matrix of the requested size and fills it with ones.
ones :: CVElement (ElemT c e) => Int -> Int -> ElemT c e -> Mat c e
ones rs cs proxy = MkMat $ c'cv_create_ones (fromIntegral rs) (fromIntegral cs) (cvElemType proxy)

-- | Allocates a matrix of the requested size and fills it with zeroes.
zeros :: CVElement (ElemT c e) => Int -> Int -> ElemT c e -> Mat c e
zeros rs cs proxy = MkMat $ c'cv_create_zeros (fromIntegral rs) (fromIntegral cs) (cvElemType proxy)

-- | Extract the number of rows in the provide matrix (equivalent to field
-- | accessor rows on the C++ side) 
rows :: Integral a => Mat c e -> a
rows m = fromIntegral $ c'cv_Mat_rows (extract m)

-- | Extract the number of rows in the provide matrix (equivalent to field
-- | accessor cols on the C++ side) 
cols :: Integral a => Mat c e -> a
cols m = fromIntegral $ c'cv_Mat_cols (extract m)

-- | Extracts a pointer to a given row, and returns a correctly typed
-- | C Array. 
rowPtr :: Storable (ElemT c e) => Mat c e -> Int -> Ptr (ElemT c e)
rowPtr m i = castPtr $ c'cv_Mat_ptr_index (extract m) (fromIntegral i)

-- | Extracts a sub matrix from a given matrix.
-- | First parameter is top left, second parameter is bottom right.
-- The submatrix given by opencv is still tied to the underlying matrix.
-- Cloning the matrix fixes this issue, and unsafePerformIO works because
-- this API does not mutate the new (or old) matrix.
subMat :: Mat c e -> V2 Int -> V2 Int -> Mat c e
subMat m (V2 i j) (V2 k l) = MkMat . S.unsafePerformIO $ c'cv_Mat_clone m''
                              where
                                m' = c'cv_Mat_getRowRange (extract m) (fromIntegral i) (fromIntegral k)
                                m'' = c'cv_Mat_getColRange m' (fromIntegral j) (fromIntegral l)

-- | Index into a matrix at (row, column) given by (V2 row column).
-- | Getter
-- The matrices exposed by this API are immutable so unsafePerformIO is
-- truly safe in this case.
getAt :: Storable (ElemT c e) => V2 Int -> Mat c e -> ElemT c e
getAt (V2 i j) m = S.unsafePerformIO $ peekElemOff (rowPtr m i) j

-- | Index into a matrix at (row, column) given by (V2 row column).
-- | Setter
-- This function probably has the most important use of unsafePerformIO
-- It's safe to use because a copy of the underlying data is made
-- before setting the pixel value. But until rewrite rules are in place
-- to eliminate unnecessary copying, this remains an inefficient approach.
-- We'd in fact prefer if the underlying platform only executed this code
-- once for any given set of arguments and never bothered to re-execute
-- this method (instead caching the result), but of course, that can't
-- be guaranteed.
setAt :: Storable (ElemT c e) => V2 Int -> Mat c e -> ElemT c e -> Mat c e
setAt (V2 i j) m e = S.unsafePerformIO $ do 
                        m' <- runCV $ clone m
                        pokeElemOff (rowPtr m' i) j e
                        return m'

-- | Lens for indexed access into a Mat. The matrix and element are always
-- | in CV because getting and setting can't be done safely outside of
-- | a monad.
pixel :: Storable (ElemT c e) => V2 Int -> Lens' (Mat c e) (ElemT c e)
pixel i = lens (getAt i) (setAt i)

-- | provides the 8/24/48/etc. neighborhood around a pixel (including the pixel
-- itself). The first parameter is the max pixel distance from 
getNeighborhood :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e)), CVElement (ElemT c e)) => Int -> V2 Int -> Mat c e -> VS.Vector (VS.Vector (ElemT c e))
getNeighborhood s (V2 i j) m = subMat m tl (br rs cs) ^. asVector
                                    where clampedLower k = if k < 0 then 0 else k
                                          clampedHigher b k = if k >= b then b else k
                                          tl = V2 (clampedLower $ i - s) (clampedLower $ j - s)
                                          br r c = V2 (clampedHigher r $ i + s) (clampedHigher c $ i + s)
                                          rs = rows m
                                          cs = cols m

-- Convenience function to index a nested vector by a Linear.V2. It
-- simplifies some code in this module and makes a whole lot of stuff
-- easier to read.
(!!!) :: (Storable (VS.Vector a), Storable a) => VS.Vector (VS.Vector a) -> V2 Int -> a
v' !!! (V2 i' j') = v' VS.! i' VS.! j'

-- | Sets the entire neighborhood for a pixel. This is a relatively
-- | inefficient function because we can't just write the entire vector at
-- | once and instead have to write each element individually. It's now
-- | triply inefficient because the matrix is deep copied for every pixel
-- | set.
setNeighborhood :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Int -> V2 Int -> Mat c e -> VS.Vector (VS.Vector (ElemT c e)) -> Mat c e
setNeighborhood s (V2 i j) m v = VS.foldl' (\m' k -> m' & pixel k .~ v !!! k) m inds  
                                    where inds = VS.fromList [V2 x y | x <- [(i - s) .. (i + s)]
                                                                           , y <- [(j - s) .. (j + s)] 
                                                                           , x >= 0 && x < (cols m)
                                                                           , y >= 0 && y < (rows m)]

-- | Lens to the 8/24/48/etc. neighborhood of a pixel (including the pixel
-- | itself).
neighborhood :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e)), CVElement (ElemT c e)) => Int -> V2 Int -> Lens' (Mat c e) (VS.Vector (VS.Vector (ElemT c e)))
neighborhood s i = lens (getNeighborhood s i) (setNeighborhood s i)

-- | Create a Vector (of Vectors)
-- | (provided for integration with linear)
-- unsafePerformIO works here because the resulting vector is frozen and
-- suffers no mutations. There's also a performance advantage here since
-- we don't want to repeatedly run this function, once is always enough on
-- any given input.
fromMat :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Mat c e -> VS.Vector (VS.Vector (ElemT c e))
fromMat m = S.unsafePerformIO $ do
                    let rs = rows m
                    let cs = cols m
                    VS.forM (VS.fromList [0 .. (rs-1)]) $ \i -> do
                      let p = rowPtr m i
                      p' <- newForeignPtr_ p
                      return $ VS.unsafeFromForeignPtr0 p' cs

-- | Turn a Vector (of Vectors) into a Mat
-- | This function lets you create a matrix intialized with the values from
-- | the Vector of vectors.
-- The Mat created this way is safe from mutation and is always equivalent
-- to the Storable Vector passed in.
toMat :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e)), CVElement (ElemT c e)) => VS.Vector (VS.Vector (ElemT c e)) -> Mat c e
toMat v = MkMat . S.unsafePerformIO $ VS.unsafeWith v (return . makeMat)
              where makeMat p = c'cv_create_Mat_with_data (fromIntegral rs) (fromIntegral cs) (cvElemType e) p 
                    rs = VS.length v
                    cs = VS.length $ v VS.! 0
                    e = v !!! (V2 0 0)

-- | Convenience Iso to convert Mats to and from Storable Vectors.
asVector :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e)), CVElement (ElemT c e)) => Iso' (Mat c e) (VS.Vector (VS.Vector (ElemT c e)))
asVector = iso fromMat toMat

-- | Create a single long Vector (lazily)
asSingleVector :: (Storable (ElemT c e), Storable (VS.Vector (ElemT c e))) => Mat c e -> VS.Vector (ElemT c e)
asSingleVector m = VS.foldr (VS.++) VS.empty $ fromMat m 

-- | The decomposition method used to compute the inverse
data InverseMethod = LU | Cholesky | SVD

inverseMethod :: InverseMethod -> CInt
inverseMethod LU = c'CV_DECOMP_LU0
inverseMethod Cholesky = c'CV_DECOMP_CHOLESKY0
inverseMethod SVD = c'CV_DECOMP_SVD0

-- | Inverts the given matrix. Uses SVD to guarantee that *something* is
-- | always returned. The matrix provided is guaranteed to at least be a
-- | pseudo-inverse (left or right). Gives a true inverse when the matrix
-- | is square and non-singular.
invert :: MatExpr c e -> MatExpr c e
invert = invertBy SVD

-- | Inverts the given matrix with the provided method. This function is
-- | partial if the method passed is not SVD *and* a singular or non-square
-- | matrix is passed in.
invertBy :: InverseMethod -> MatExpr c e -> MatExpr c e
invertBy im m = MkMatExpr $ c'cv_Mat_inv_mat (extractExpr m) (inverseMethod im)

transpose :: MatExpr c e -> MatExpr c e
transpose = MkMatExpr . c'cv_Mat_transpose_mat . extractExpr

-- | Forces the evaluation of an accumulated Matrix Expression.
force :: MatExpr c e -> Mat c e
force m = MkMat . S.unsafePerformIO $ c'force (extractExpr m) >>= c'cv_Mat_clone

-- | Promotes a matrix to a matrix expression. Think (\m -> (\() -> m)).
promote :: Mat c e -> MatExpr c e
promote = MkMatExpr . c'promote . extract

-- | Convenience Iso for promotion 
promoting :: Iso (Mat c e) (Mat c e) (MatExpr c e) (MatExpr c e)
promoting = iso promote force

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
