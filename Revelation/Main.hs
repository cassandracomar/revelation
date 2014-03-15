{-# LANGUAGE DataKinds #-}
module Main where
import Revelation
import Linear
import Control.Monad

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture RGB e)
       >-> convertColorP
       >-> indexP (V2 0 0)
       >-> waitKeyP (Just 'q') 10 
       >-> (imageDisplayWindow "Test" :: Window Grayscale e)

