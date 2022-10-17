{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import           Data.Void
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.IO           (unsafePerformIO)

foreign import ccall "wrapper"
  wrap_cint :: (CInt -> CInt) -> IO (FunPtr (CInt -> CInt))

foreign import ccall "lib.h create_table" rust_create_table :: Ptr CChar -> Int -> IO (Ptr Int)
foreign import ccall "lib.h run" rust_run :: Ptr Int -> IO ()
foreign import ccall "lib.h add_func_int_32" rust_add_func_int_32 :: Ptr Int -> FunPtr Int -> FunPtr Int -> IO ()

data TableGen where
  TableGen :: (IO (Ptr Int)) -> TableGen
instance Show TableGen where
  show :: TableGen -> String
  show (TableGen ptr) = "TableGen " ++ show (unsafePerformIO ptr)

createTable :: String -> TableGen
createTable msg = TableGen $ newCAStringLen msg >>= uncurry rust_create_table

runTable :: TableGen -> IO ()
runTable (TableGen ptr) = ptr >>= rust_run

addCIntFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CInt) ->  IO()
addCIntFuncs (TableGen ptr) p1 p2 = (ptr >>= rust_add_func_int_32) (wrap p1) (wrap p2)

main :: IO ()
main = do
    let table = createTable "Howdy from Haskell!"

    addCIntFuncs table day1 day2

    runTable table

day1 x = 21312
day2 x = 421
