import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = \a b -> generateBindings b >> preConf simpleUserHooks a b
                                            -- The following hook prevents cabal from checking to see if generated cpp header and
                                            -- source files actually compile before completing configuration.
                                            , postConf = postConf emptyUserHooks
                                            , postClean = \a b c d -> cleanBindings b >> postClean simpleUserHooks a b c d  }

generateBindings :: ConfigFlags -> IO ()
generateBindings flags = rawSystemExit (fromFlag $ configVerbosity flags) "python"
                        ["cbits/genhsc.py", "/usr/local/include", "cbits/", "Revelation/Bindings/", "opencv2/core/core.hpp", "opencv2/flann/miniflann.hpp", "opencv2/ml/ml.hpp", "opencv2/imgproc/imgproc.hpp", "opencv2/calib3d/calib3d.hpp", "opencv2/features2d/features2d.hpp", "opencv2/video/tracking.hpp", "opencv2/video/background_segm.hpp", "opencv2/objdetect/objdetect.hpp", "opencv2/contrib/contrib.hpp", "opencv2/highgui/highgui.hpp"]

cleanBindings :: CleanFlags -> IO ()
cleanBindings flags = rawSystemExit (fromFlag $ cleanVerbosity flags) "rm"
                          ["-f", "cbits/opencv_generated.cpp", "cbits/opencv_generated.hpp", "Revelation/Bindings/RawTypes.hsc", "Revelation/Bindings/RawConsts.hsc", "Revelation/Bindings/RawFuncs.hsc"]

