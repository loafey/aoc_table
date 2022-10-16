{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.Void

foreign import ccall "wrapper"
  wrap :: (CLong -> CLong) -> IO (FunPtr (CLong -> CLong))

foreign import ccall "lib.h create_table" create_table :: Ptr CChar -> Int -> IO (Ptr Int)
foreign import ccall "lib.h run" run :: Ptr Int -> IO ()
foreign import ccall "lib.h add_func" add_func :: Ptr Int -> (FunPtr (a)) -> (FunPtr (b)) -> IO ()

main :: IO ()
main = do
    (msgPtr, msgLen) <- newCStringLen "Howdy from Haskell!"

    table <- create_table msgPtr msgLen
    p1 <- wrap day1
    p2 <- wrap day2

    add_func table p1 p2
    run table

day1 x = 21312
day2 x = 421