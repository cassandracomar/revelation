{-# LANGUAGE DataKinds #-}
module Main where
import Revelation

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture d RGB e)
       >-> convertColor 
       >-> waitKey (Just 'q') 10 
       >-> (imageDisplayWindow "Test" :: Window d Grayscale e)

