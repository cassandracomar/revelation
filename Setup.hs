import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.PackageDescription

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preConf = \a b -> generateCPP a b >> preConf simpleUserHooks a b 
                                            , postClean = \a b c d -> cleanCPP a b c >> postClean simpleUserHooks a b c d }
                        
generateCPP :: Args -> ConfigFlags -> IO ()
generateCPP _ flags = rawSystemExit (fromFlag $ configVerbosity flags) "env"
                        ["python", "cbits/genc.py", "cbits/"]

cleanCPP :: Args -> CleanFlags -> PackageDescription -> IO () 
cleanCPP _ flags _ = rawSystemExit (fromFlag $ cleanVerbosity flags) "env"
                          ["rm", "-f", "cbits/opencv_generated.cpp", "cbits/opencv_generated.hpp"]
