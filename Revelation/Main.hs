{-# LANGUAGE DataKinds #-}
module Main where
import Revelation
import Linear
import Data.Word

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture RGB Word8)
       >-> indexP (V2 0 0)
       >-> waitKeyP (Just 'q') 10 
       >-> (imageDisplayWindow "Test" :: Window RGB e)

