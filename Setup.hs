import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = \a b -> generateCPP b >> preConf simpleUserHooks a b
                                            -- The following hook prevents cabal from checking to see if generated cpp header and
                                            -- source files actually compile before completing configuration. 
                                            , postConf = postConf emptyUserHooks
                                            , postClean = \a b c d -> cleanCPP b >> postClean simpleUserHooks a b c d }

generateCPP :: ConfigFlags -> IO ()
generateCPP flags = rawSystemExit (fromFlag $ configVerbosity flags) "python"
                        ["cbits/genc.py", "cbits/"]

cleanCPP :: CleanFlags -> IO ()
cleanCPP flags = rawSystemExit (fromFlag $ cleanVerbosity flags) "rm"
                          ["-f", "cbits/opencv_generated.cpp", "cbits/opencv_generated.hpp"]
