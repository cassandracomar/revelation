{-# LANGUAGE DataKinds #-}
module Main where
import Revelation
import Data.Word

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture m n RGB Word8)
       >-> convertColorP
       >-> waitKeyP (Just 'q') 1 
       >-> (imageDisplayWindow "Test" :: Window m n Grayscale e)

