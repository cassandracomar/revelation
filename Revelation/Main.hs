{-# LANGUAGE DataKinds #-}
module Main where
import Revelation
import Data.Word

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture RGB Word8)
       >-> convertColorP
       >-> waitKeyP (Just 'q') 10 
       >-> (imageDisplayWindow "Test" :: Window Grayscale e)

