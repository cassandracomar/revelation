{-# LANGUAGE DataKinds #-}
module Main where
import Revelation
import Linear
import Data.Word

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture RGB e)
       >-> convertColorP
       >-> (indexP (V2 0 0) :: Pipe (Mat Grayscale Word8) (Mat Grayscale Word8) CV ())
       >-> waitKeyP (Just 'q') 10 
       >-> (imageDisplayWindow "Test" :: Window Grayscale e)

