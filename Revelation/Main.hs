module Main where
import Revelation.Bindings.RawTypes
import Revelation.Bindings.RawFuncs
import Revelation.Bindings.Mat
import Revelation.Video
import Foreign.C
import Control.Monad
import Pipes


waitKey :: Maybe Char -> Int -> Pipe Mat Mat IO ()
waitKey Nothing n =  do mat <- await
                        lift $ c'cv_waitKey (fromIntegral n)
                        yield mat
                        waitKey Nothing n
waitKey (Just c) n = do mat <- await
                        cchar <- lift $ c'cv_waitKey (fromIntegral n)
                        when (castCCharToChar (fromIntegral cchar) /= c) $ do 
                          yield mat
                          waitKey (Just c) n

main :: IO ()
main = runEffect $ cameraCapture 0 >-> waitKey (Just 'q') 10 >-> imageDisplayWindow "Test"

