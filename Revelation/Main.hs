{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Main where
import Foreign.C

foreign import ccall "fib" fib :: CInt -> IO CInt

main :: IO ()
main = fib 10 >>= print
