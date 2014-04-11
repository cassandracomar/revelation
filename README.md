Revelation
==========

This is a computer vision library for Haskell using OpenCV as a backend.

This is done by providing access to the C++ functions and methods via export "C" wrappers,
and using bindings-dsl macros to make the functions available in Haskell. These low-level bindings are aimed for inclusion in the 3.0
release of OpenCV.

This is very much a WIP, and pull requests / issues / feature requests are very very welcome.

The raw bindings have been integrated into OpenCV which you can find in my fork [here](https://github.com/arjuncomar/opencv_contrib.git).
Building that fork will install a package called opencv that will provide the raw bindings necessary for this package.

Compilation / Installation
------------

This project currently requires GHC 7.8. This package requires opencv installed from [my fork](https://github.com/arjuncomar/opencv_contrib.git) -- follow the instructions on the readme to compile the
library with the [main opencv tree](https://github.com/itseez/opencv.git). Let me know if you have 
any trouble building it. This library should build easily and relatively painlessly. 

Submit any issues you have building the library!

Linking
-----------

Linking executables built using this library is a little painful. The reason isn't entirely clear to me, but for some reason, you can't link against the shared libopencv_c.so archive -- symbols are just never found. Linking instead against the static libopencv_c.a solves this problem, but cabal doesn't have a good story for that kind of linking. Using --whole-archive and -Bstatic as linker options allows ld to correctly link executables regardless of where cabal sticks them on the command line, but getting the details to work is tricky.

If you need to link an executable against this library, include the following in your ghc-options field in your cabal file:

    ghc-options: -pgml g++ "-optl-Wl,--whole-archive" "-optl-Wl,-Bstatic" "-optl-Wl,-lopencv_c" "-optl-Wl,-Bdynamic" "-optl-Wl,--no-whole-archive"

This allows the executable to correctly link. Execution will still require the opencv libs to be on the LD_LIBRARY_PATH
so make sure that variable is set correctly as well. If you have any issues with this process let me know and I'll
help you through it.

I'll update this with instructions for OS X and Windows as I test in those environments.

Design Goals
-----------

This API is very much under development. The idea is to provide the major OpenCV functionality via Pipes. This works for a couple of
reasons. First, the OpenCV library is for the most part not referentially transparent, and it is not safe to bring most of the
functionality into the pure subset of Haskell via unsafePerformIO. Therefore we're stuck inside IO for pretty much any non-trivial
program, and we can't easily separate the vision code into pure and impure portions. Pipes helps here by providing a more compositional
(and less surprising) interface for effectful programming.

Second, if you pick a random CV paper and read through it, you'll find something rather interesting -- most CV algorithms are described
as pipelines with several discrete stages. This means translating these algorithms to code is more natural when the building blocks
you're given fit together in pipelines.

However this kind of API doesn't work well for the many image processing problems for which OpenCV is used. For these kinds of algorithms,
a standard monadic (mutable) API is under way, and a pure API (via copying) for those that prefer it.

More description and explanation on how to use this API will follow as it's developed further and its semantics are established and 
stabilized.
