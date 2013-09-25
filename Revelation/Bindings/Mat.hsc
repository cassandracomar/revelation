{-# LANGUAGE ForeignFunctionInterface #-}
#include <bindings.dsl.h>
#include <mat.hpp>
module Revelation.Bindings.Mat where
#strict_import

import Revelation.Bindings.RawTypes
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr

#ccall cv_create_Mat        , IO (Ptr <Mat>)
#ccall cv_Mat_assign        , Ptr <Mat> -> Ptr <Mat> -> IO (Ptr <Mat>) 
#ccall cv_Mat_assignVal     , Ptr <Mat> -> Ptr <Scalar> -> IO (Ptr <Mat>) 

#ccall cv_Mat_getRow        , Ptr <Mat> -> CInt -> IO (Ptr <Mat>)
#ccall cv_Mat_getCol        , Ptr <Mat> -> CInt -> IO (Ptr <Mat>)
#ccall cv_Mat_getRowRange   , Ptr <Mat> -> CInt -> CInt -> IO (Ptr <Mat>)
#ccall cv_Mat_getColRange   , Ptr <Mat> -> CInt -> CInt -> IO (Ptr <Mat>)

#ccall cv_Mat_diag          , Ptr <Mat> -> IO (Ptr <Mat>)
#ccall cv_Mat_diag_d        , Ptr <Mat> -> CInt -> IO (Ptr <Mat>)
#ccall cv_create_diagMat    , Ptr <Mat> -> IO (Ptr <Mat>)

#ccall cv_Mat_clone         , Ptr <Mat> -> IO (Ptr <Mat>)
#ccall cv_Mat_copyTo        , Ptr <Mat> -> Ptr <Mat> -> IO ()
#ccall cv_Mat_copyTo_masked , Ptr <Mat> -> Ptr <Mat> -> Ptr <Mat> -> IO ()
#ccall cv_Mat_assignTo      , Ptr <Mat> -> Ptr <Mat> -> IO ()
#ccall cv_Mat_assignTo_t    , Ptr <Mat> -> Ptr <Mat> -> CInt -> IO ()
#ccall cv_Mat_setTo         , Ptr <Mat> -> Ptr <Scalar> -> IO (Ptr <Mat>)
#ccall cv_Mat_setTo_masked  , Ptr <Mat> -> Ptr <Scalar> -> Ptr <Mat> -> IO (Ptr <Mat>)

#ccall cv_Mat_reshape       , Ptr <Mat> -> CInt -> IO (Ptr <Mat>)
#ccall cv_Mat_reshape_rows  , Ptr <Mat> -> CInt -> CInt -> IO (Ptr <Mat>)
