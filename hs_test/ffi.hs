{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.IO           (unsafePerformIO)

foreign import ccall "wrapper"
  wrap_cint :: (CInt -> CInt) -> IO (FunPtr (CInt -> CInt))

data TableGen where
  TableGen :: (IO (Ptr Int)) -> TableGen
instance Show TableGen where
  show :: TableGen -> String
  show (TableGen ptr) = "TableGen " ++ show (unsafePerformIO ptr)

foreign import ccall "lib.h create_table" rust_create_table :: Ptr CChar -> Int -> IO (Ptr Int)
createTable :: String -> TableGen
createTable msg = TableGen $ newCAStringLen msg >>= uncurry rust_create_table

foreign import ccall "lib.h run" rust_run :: Ptr Int -> IO ()
runTable :: TableGen -> IO ()
runTable (TableGen ptr) = ptr >>= rust_run

foreign import ccall "lib.h add_func_int_32" rust_add_func_int_32 :: 
                            Ptr Int ->
                            FunPtr (CInt -> CInt) ->
                            FunPtr (CInt -> CInt) ->
                            IO ()
addCIntFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CInt) ->  IO()
addCIntFuncs (TableGen ptr) p1 p2 = do
  ptr' <- ptr
  p1' <- wrap_cint p1
  p2' <- wrap_cint p2
  rust_add_func_int_32 ptr' p1' p2'

main :: IO ()
main = do
    let table = createTable "Howdy from Haskell!"

    addCIntFuncs table day1 day2

    runTable table

day1 x = 21312
day2 x = 421
