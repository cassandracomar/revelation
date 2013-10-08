{-# LANGUAGE DataKinds #-}
module Main where
import Revelation
import Linear
import System.Environment
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
     []    -> error "Usage: CaptureFromFile path/to/file.avi"
     (fName:_) -> 
       runCV . runEffect $
       (fileCapture fName :: VideoCapture RGB e)
       >-> convertColorP
       >-> indexP
       >-> waitKeyP (Just 'q') 30
       >-> (imageDisplayWindow "Test" :: Window Grayscale e)