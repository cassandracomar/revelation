{-# LANGUAGE DataKinds #-}
module Main where
import Revelation.Video
import Revelation.UI
import Revelation.Color
import Revelation.Mat
import Pipes

main :: IO ()
main = runCV . runEffect $ 
       (cameraCapture 0 :: VideoCapture d RGB e)
       >-> convertColor 
       >-> waitKey (Just 'q') 10 
       >-> (imageDisplayWindow "Test" :: Window d Grayscale e)

