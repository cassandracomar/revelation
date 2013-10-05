Revelation
==========

This is a computer vision library for Haskell using OpenCV as a backend.

This is done by providing access to the C++ functions and methods via export "C" wrappers,
and using bindings-dsl macros to make the functions available in Haskell. These low-level bindings are aimed for inclusion in the 3.0
release of OpenCV.

This is very much a WIP, and pull requests / issues / feature requests are very very welcome.

I've moved the raw bindings to [opencv-raw](https://github.com/arjuncomar/opencv-raw.git), and you can help me integrate them into
OpenCV [here](https://github.com/arjuncomar/opencv.git).

Compilation / Installation
------------

This project currently requires GHC 7.6 (though it might work with 7.4). It also requires opencv-raw which takes a little effort
to get working. This library should build easily and relatively painlessly. 

Submit any issues you have building the library!

A New Kind of Computer Vision Library
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
