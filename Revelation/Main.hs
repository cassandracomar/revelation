module Main where
import Revelation.Bindings.RawConsts
import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawFuncs
import Revelation.Bindings.CppTypes
import Revelation.Bindings.Mat
import Foreign.C
import Foreign.C.Types
import Control.Monad

main :: IO ()
main = do capture     <- c'cv_create_VideoCapture1 0
          
          window_name <- toStdString "Test"
          c'cv_namedWindow window_name c'CV_WINDOW_NORMAL0

          img <- c'cv_create_Mat

          forM_ [1..1000] $ \_ -> do
            c'cv_VideoCapture_read capture img
            c'cv_imshow window_name img
            c'cv_waitKey 1
