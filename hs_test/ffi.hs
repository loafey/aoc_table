{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import           Data.Void
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall "wrapper"
  wrap :: (CLong -> CLong) -> IO (FunPtr (CLong -> CLong))

foreign import ccall "lib.h create_table" rust_create_table :: Ptr CChar -> Int -> IO (Ptr Int)
foreign import ccall "lib.h run" rust_run :: Ptr Int -> IO ()
foreign import ccall "lib.h add_func" rust_add_func :: Ptr Int -> FunPtr a -> FunPtr b -> IO ()

createTable :: String -> IO (Ptr Int)
createTable msg = newCAStringLen msg >>= uncurry rust_create_table

main :: IO ()
main = do
    (msgPtr, msgLen) <- newCStringLen "Howdy from Haskell!"

    table <- rust_create_table msgPtr msgLen
    p1 <- wrap day1
    p2 <- wrap day2

    rust_add_func table p1 p2
    rust_run table

day1 x = 21312
day2 x = 421
