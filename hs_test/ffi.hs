{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.IO           (unsafePerformIO)

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

main :: IO ()
main = do
    let table = createTable "Howdy from Haskell!"
    let table' = addCIntCBoolFuncs table (\x -> sum [0..100]) (const 1)
    let table'' = addCFloatCIntFuncs table' (const 0.2431) (const 1)
    runTable table''

----- auto -----

foreign import ccall "wrapper" wrapCBool :: (CInt -> CBool) -> IO (FunPtr (CInt -> CBool))
foreign import ccall "wrapper" wrapCFloat :: (CInt -> CFloat) -> IO (FunPtr (CInt -> CFloat))
foreign import ccall "wrapper" wrapCDouble :: (CInt -> CDouble) -> IO (FunPtr (CInt -> CDouble))
foreign import ccall "wrapper" wrapCShort :: (CInt -> CShort) -> IO (FunPtr (CInt -> CShort))
foreign import ccall "wrapper" wrapCInt :: (CInt -> CInt) -> IO (FunPtr (CInt -> CInt))
foreign import ccall "wrapper" wrapCLong :: (CInt -> CLong) -> IO (FunPtr (CInt -> CLong))
foreign import ccall "wrapper" wrapCSChar :: (CInt -> CSChar) -> IO (FunPtr (CInt -> CSChar))
foreign import ccall "wrapper" wrapCUShort :: (CInt -> CUShort) -> IO (FunPtr (CInt -> CUShort))
foreign import ccall "wrapper" wrapCUInt :: (CInt -> CUInt) -> IO (FunPtr (CInt -> CUInt))
foreign import ccall "wrapper" wrapCULong :: (CInt -> CULong) -> IO (FunPtr (CInt -> CULong))
foreign import ccall "wrapper" wrapCUChar :: (CInt -> CUChar) -> IO (FunPtr (CInt -> CUChar))
foreign import ccall "lib.h add_func_bool_bool" rust_add_func_bool_bool :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_f32" rust_add_func_bool_f32 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_f64" rust_add_func_bool_f64 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i16" rust_add_func_bool_i16 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i32" rust_add_func_bool_i32 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i64" rust_add_func_bool_i64 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i8" rust_add_func_bool_i8 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u16" rust_add_func_bool_u16 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u32" rust_add_func_bool_u32 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u64" rust_add_func_bool_u64 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u8" rust_add_func_bool_u8 :: Ptr Int -> FunPtr (CInt -> CBool) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_bool" rust_add_func_f32_bool :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_f32" rust_add_func_f32_f32 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_f64" rust_add_func_f32_f64 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i16" rust_add_func_f32_i16 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i32" rust_add_func_f32_i32 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i64" rust_add_func_f32_i64 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i8" rust_add_func_f32_i8 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u16" rust_add_func_f32_u16 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u32" rust_add_func_f32_u32 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u64" rust_add_func_f32_u64 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u8" rust_add_func_f32_u8 :: Ptr Int -> FunPtr (CInt -> CFloat) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_bool" rust_add_func_f64_bool :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_f32" rust_add_func_f64_f32 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_f64" rust_add_func_f64_f64 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i16" rust_add_func_f64_i16 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i32" rust_add_func_f64_i32 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i64" rust_add_func_f64_i64 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i8" rust_add_func_f64_i8 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u16" rust_add_func_f64_u16 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u32" rust_add_func_f64_u32 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u64" rust_add_func_f64_u64 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u8" rust_add_func_f64_u8 :: Ptr Int -> FunPtr (CInt -> CDouble) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_bool" rust_add_func_i16_bool :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_f32" rust_add_func_i16_f32 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_f64" rust_add_func_i16_f64 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i16" rust_add_func_i16_i16 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i32" rust_add_func_i16_i32 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i64" rust_add_func_i16_i64 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i8" rust_add_func_i16_i8 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u16" rust_add_func_i16_u16 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u32" rust_add_func_i16_u32 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u64" rust_add_func_i16_u64 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u8" rust_add_func_i16_u8 :: Ptr Int -> FunPtr (CInt -> CShort) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_bool" rust_add_func_i32_bool :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_f32" rust_add_func_i32_f32 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_f64" rust_add_func_i32_f64 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i16" rust_add_func_i32_i16 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i32" rust_add_func_i32_i32 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i64" rust_add_func_i32_i64 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i8" rust_add_func_i32_i8 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u16" rust_add_func_i32_u16 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u32" rust_add_func_i32_u32 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u64" rust_add_func_i32_u64 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u8" rust_add_func_i32_u8 :: Ptr Int -> FunPtr (CInt -> CInt) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_bool" rust_add_func_i64_bool :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_f32" rust_add_func_i64_f32 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_f64" rust_add_func_i64_f64 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i16" rust_add_func_i64_i16 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i32" rust_add_func_i64_i32 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i64" rust_add_func_i64_i64 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i8" rust_add_func_i64_i8 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u16" rust_add_func_i64_u16 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u32" rust_add_func_i64_u32 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u64" rust_add_func_i64_u64 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u8" rust_add_func_i64_u8 :: Ptr Int -> FunPtr (CInt -> CLong) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_bool" rust_add_func_i8_bool :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_f32" rust_add_func_i8_f32 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_f64" rust_add_func_i8_f64 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i16" rust_add_func_i8_i16 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i32" rust_add_func_i8_i32 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i64" rust_add_func_i8_i64 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i8" rust_add_func_i8_i8 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u16" rust_add_func_i8_u16 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u32" rust_add_func_i8_u32 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u64" rust_add_func_i8_u64 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u8" rust_add_func_i8_u8 :: Ptr Int -> FunPtr (CInt -> CSChar) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_bool" rust_add_func_u16_bool :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_f32" rust_add_func_u16_f32 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_f64" rust_add_func_u16_f64 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i16" rust_add_func_u16_i16 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i32" rust_add_func_u16_i32 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i64" rust_add_func_u16_i64 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i8" rust_add_func_u16_i8 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u16" rust_add_func_u16_u16 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u32" rust_add_func_u16_u32 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u64" rust_add_func_u16_u64 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u8" rust_add_func_u16_u8 :: Ptr Int -> FunPtr (CInt -> CUShort) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_bool" rust_add_func_u32_bool :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_f32" rust_add_func_u32_f32 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_f64" rust_add_func_u32_f64 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i16" rust_add_func_u32_i16 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i32" rust_add_func_u32_i32 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i64" rust_add_func_u32_i64 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i8" rust_add_func_u32_i8 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u16" rust_add_func_u32_u16 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u32" rust_add_func_u32_u32 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u64" rust_add_func_u32_u64 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u8" rust_add_func_u32_u8 :: Ptr Int -> FunPtr (CInt -> CUInt) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_bool" rust_add_func_u64_bool :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_f32" rust_add_func_u64_f32 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_f64" rust_add_func_u64_f64 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i16" rust_add_func_u64_i16 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i32" rust_add_func_u64_i32 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i64" rust_add_func_u64_i64 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i8" rust_add_func_u64_i8 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u16" rust_add_func_u64_u16 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u32" rust_add_func_u64_u32 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u64" rust_add_func_u64_u64 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u8" rust_add_func_u64_u8 :: Ptr Int -> FunPtr (CInt -> CULong) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_bool" rust_add_func_u8_bool :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CBool) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_f32" rust_add_func_u8_f32 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CFloat) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_f64" rust_add_func_u8_f64 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CDouble) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i16" rust_add_func_u8_i16 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i32" rust_add_func_u8_i32 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i64" rust_add_func_u8_i64 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CLong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i8" rust_add_func_u8_i8 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CSChar) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u16" rust_add_func_u8_u16 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CUShort) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u32" rust_add_func_u8_u32 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CUInt) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u64" rust_add_func_u8_u64 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CULong) -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u8" rust_add_func_u8_u8 :: Ptr Int -> FunPtr (CInt -> CUChar) -> FunPtr (CInt -> CUChar) -> IO (Ptr Int)
addCBoolCBoolFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CBool) -> TableGen
addCBoolCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_bool_bool ptr' p1' p2'
addCBoolCFloatFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CFloat) -> TableGen
addCBoolCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_bool_f32 ptr' p1' p2'
addCBoolCDoubleFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CDouble) -> TableGen
addCBoolCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_bool_f64 ptr' p1' p2'
addCBoolCShortFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CShort) -> TableGen
addCBoolCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_bool_i16 ptr' p1' p2'
addCBoolCIntFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CInt) -> TableGen
addCBoolCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_bool_i32 ptr' p1' p2'
addCBoolCLongFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CLong) -> TableGen
addCBoolCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_bool_i64 ptr' p1' p2'
addCBoolCSCharFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CSChar) -> TableGen
addCBoolCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_bool_i8 ptr' p1' p2'
addCBoolCUShortFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CUShort) -> TableGen
addCBoolCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_bool_u16 ptr' p1' p2'
addCBoolCUIntFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CUInt) -> TableGen
addCBoolCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_bool_u32 ptr' p1' p2'
addCBoolCULongFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CULong) -> TableGen
addCBoolCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_bool_u64 ptr' p1' p2'
addCBoolCUCharFuncs :: TableGen -> (CInt -> CBool) -> (CInt -> CUChar) -> TableGen
addCBoolCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_bool_u8 ptr' p1' p2'
addCFloatCBoolFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CBool) -> TableGen
addCFloatCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_f32_bool ptr' p1' p2'
addCFloatCFloatFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CFloat) -> TableGen
addCFloatCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_f32_f32 ptr' p1' p2'
addCFloatCDoubleFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CDouble) -> TableGen
addCFloatCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_f32_f64 ptr' p1' p2'
addCFloatCShortFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CShort) -> TableGen
addCFloatCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_f32_i16 ptr' p1' p2'
addCFloatCIntFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CInt) -> TableGen
addCFloatCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_f32_i32 ptr' p1' p2'
addCFloatCLongFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CLong) -> TableGen
addCFloatCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_f32_i64 ptr' p1' p2'
addCFloatCSCharFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CSChar) -> TableGen
addCFloatCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_f32_i8 ptr' p1' p2'
addCFloatCUShortFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CUShort) -> TableGen
addCFloatCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_f32_u16 ptr' p1' p2'
addCFloatCUIntFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CUInt) -> TableGen
addCFloatCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_f32_u32 ptr' p1' p2'
addCFloatCULongFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CULong) -> TableGen
addCFloatCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_f32_u64 ptr' p1' p2'
addCFloatCUCharFuncs :: TableGen -> (CInt -> CFloat) -> (CInt -> CUChar) -> TableGen
addCFloatCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_f32_u8 ptr' p1' p2'
addCDoubleCBoolFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CBool) -> TableGen
addCDoubleCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_f64_bool ptr' p1' p2'
addCDoubleCFloatFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CFloat) -> TableGen
addCDoubleCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_f64_f32 ptr' p1' p2'
addCDoubleCDoubleFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CDouble) -> TableGen
addCDoubleCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_f64_f64 ptr' p1' p2'
addCDoubleCShortFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CShort) -> TableGen
addCDoubleCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_f64_i16 ptr' p1' p2'
addCDoubleCIntFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CInt) -> TableGen
addCDoubleCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_f64_i32 ptr' p1' p2'
addCDoubleCLongFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CLong) -> TableGen
addCDoubleCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_f64_i64 ptr' p1' p2'
addCDoubleCSCharFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CSChar) -> TableGen
addCDoubleCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_f64_i8 ptr' p1' p2'
addCDoubleCUShortFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CUShort) -> TableGen
addCDoubleCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_f64_u16 ptr' p1' p2'
addCDoubleCUIntFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CUInt) -> TableGen
addCDoubleCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_f64_u32 ptr' p1' p2'
addCDoubleCULongFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CULong) -> TableGen
addCDoubleCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_f64_u64 ptr' p1' p2'
addCDoubleCUCharFuncs :: TableGen -> (CInt -> CDouble) -> (CInt -> CUChar) -> TableGen
addCDoubleCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_f64_u8 ptr' p1' p2'
addCShortCBoolFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CBool) -> TableGen
addCShortCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i16_bool ptr' p1' p2'
addCShortCFloatFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CFloat) -> TableGen
addCShortCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i16_f32 ptr' p1' p2'
addCShortCDoubleFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CDouble) -> TableGen
addCShortCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i16_f64 ptr' p1' p2'
addCShortCShortFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CShort) -> TableGen
addCShortCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i16_i16 ptr' p1' p2'
addCShortCIntFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CInt) -> TableGen
addCShortCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i16_i32 ptr' p1' p2'
addCShortCLongFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CLong) -> TableGen
addCShortCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i16_i64 ptr' p1' p2'
addCShortCSCharFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CSChar) -> TableGen
addCShortCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i16_i8 ptr' p1' p2'
addCShortCUShortFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CUShort) -> TableGen
addCShortCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i16_u16 ptr' p1' p2'
addCShortCUIntFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CUInt) -> TableGen
addCShortCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i16_u32 ptr' p1' p2'
addCShortCULongFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CULong) -> TableGen
addCShortCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i16_u64 ptr' p1' p2'
addCShortCUCharFuncs :: TableGen -> (CInt -> CShort) -> (CInt -> CUChar) -> TableGen
addCShortCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i16_u8 ptr' p1' p2'
addCIntCBoolFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CBool) -> TableGen
addCIntCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i32_bool ptr' p1' p2'
addCIntCFloatFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CFloat) -> TableGen
addCIntCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i32_f32 ptr' p1' p2'
addCIntCDoubleFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CDouble) -> TableGen
addCIntCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i32_f64 ptr' p1' p2'
addCIntCShortFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CShort) -> TableGen
addCIntCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i32_i16 ptr' p1' p2'
addCIntCIntFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CInt) -> TableGen
addCIntCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i32_i32 ptr' p1' p2'
addCIntCLongFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CLong) -> TableGen
addCIntCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i32_i64 ptr' p1' p2'
addCIntCSCharFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CSChar) -> TableGen
addCIntCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i32_i8 ptr' p1' p2'
addCIntCUShortFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CUShort) -> TableGen
addCIntCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i32_u16 ptr' p1' p2'
addCIntCUIntFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CUInt) -> TableGen
addCIntCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i32_u32 ptr' p1' p2'
addCIntCULongFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CULong) -> TableGen
addCIntCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i32_u64 ptr' p1' p2'
addCIntCUCharFuncs :: TableGen -> (CInt -> CInt) -> (CInt -> CUChar) -> TableGen
addCIntCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i32_u8 ptr' p1' p2'
addCLongCBoolFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CBool) -> TableGen
addCLongCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i64_bool ptr' p1' p2'
addCLongCFloatFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CFloat) -> TableGen
addCLongCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i64_f32 ptr' p1' p2'
addCLongCDoubleFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CDouble) -> TableGen
addCLongCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i64_f64 ptr' p1' p2'
addCLongCShortFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CShort) -> TableGen
addCLongCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i64_i16 ptr' p1' p2'
addCLongCIntFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CInt) -> TableGen
addCLongCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i64_i32 ptr' p1' p2'
addCLongCLongFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CLong) -> TableGen
addCLongCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i64_i64 ptr' p1' p2'
addCLongCSCharFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CSChar) -> TableGen
addCLongCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i64_i8 ptr' p1' p2'
addCLongCUShortFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CUShort) -> TableGen
addCLongCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i64_u16 ptr' p1' p2'
addCLongCUIntFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CUInt) -> TableGen
addCLongCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i64_u32 ptr' p1' p2'
addCLongCULongFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CULong) -> TableGen
addCLongCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i64_u64 ptr' p1' p2'
addCLongCUCharFuncs :: TableGen -> (CInt -> CLong) -> (CInt -> CUChar) -> TableGen
addCLongCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i64_u8 ptr' p1' p2'
addCSCharCBoolFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CBool) -> TableGen
addCSCharCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i8_bool ptr' p1' p2'
addCSCharCFloatFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CFloat) -> TableGen
addCSCharCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i8_f32 ptr' p1' p2'
addCSCharCDoubleFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CDouble) -> TableGen
addCSCharCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i8_f64 ptr' p1' p2'
addCSCharCShortFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CShort) -> TableGen
addCSCharCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i8_i16 ptr' p1' p2'
addCSCharCIntFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CInt) -> TableGen
addCSCharCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i8_i32 ptr' p1' p2'
addCSCharCLongFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CLong) -> TableGen
addCSCharCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i8_i64 ptr' p1' p2'
addCSCharCSCharFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CSChar) -> TableGen
addCSCharCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i8_i8 ptr' p1' p2'
addCSCharCUShortFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CUShort) -> TableGen
addCSCharCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i8_u16 ptr' p1' p2'
addCSCharCUIntFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CUInt) -> TableGen
addCSCharCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i8_u32 ptr' p1' p2'
addCSCharCULongFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CULong) -> TableGen
addCSCharCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i8_u64 ptr' p1' p2'
addCSCharCUCharFuncs :: TableGen -> (CInt -> CSChar) -> (CInt -> CUChar) -> TableGen
addCSCharCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i8_u8 ptr' p1' p2'
addCUShortCBoolFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CBool) -> TableGen
addCUShortCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u16_bool ptr' p1' p2'
addCUShortCFloatFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CFloat) -> TableGen
addCUShortCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u16_f32 ptr' p1' p2'
addCUShortCDoubleFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CDouble) -> TableGen
addCUShortCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u16_f64 ptr' p1' p2'
addCUShortCShortFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CShort) -> TableGen
addCUShortCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u16_i16 ptr' p1' p2'
addCUShortCIntFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CInt) -> TableGen
addCUShortCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u16_i32 ptr' p1' p2'
addCUShortCLongFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CLong) -> TableGen
addCUShortCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u16_i64 ptr' p1' p2'
addCUShortCSCharFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CSChar) -> TableGen
addCUShortCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u16_i8 ptr' p1' p2'
addCUShortCUShortFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CUShort) -> TableGen
addCUShortCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u16_u16 ptr' p1' p2'
addCUShortCUIntFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CUInt) -> TableGen
addCUShortCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u16_u32 ptr' p1' p2'
addCUShortCULongFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CULong) -> TableGen
addCUShortCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u16_u64 ptr' p1' p2'
addCUShortCUCharFuncs :: TableGen -> (CInt -> CUShort) -> (CInt -> CUChar) -> TableGen
addCUShortCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u16_u8 ptr' p1' p2'
addCUIntCBoolFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CBool) -> TableGen
addCUIntCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u32_bool ptr' p1' p2'
addCUIntCFloatFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CFloat) -> TableGen
addCUIntCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u32_f32 ptr' p1' p2'
addCUIntCDoubleFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CDouble) -> TableGen
addCUIntCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u32_f64 ptr' p1' p2'
addCUIntCShortFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CShort) -> TableGen
addCUIntCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u32_i16 ptr' p1' p2'
addCUIntCIntFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CInt) -> TableGen
addCUIntCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u32_i32 ptr' p1' p2'
addCUIntCLongFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CLong) -> TableGen
addCUIntCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u32_i64 ptr' p1' p2'
addCUIntCSCharFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CSChar) -> TableGen
addCUIntCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u32_i8 ptr' p1' p2'
addCUIntCUShortFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CUShort) -> TableGen
addCUIntCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u32_u16 ptr' p1' p2'
addCUIntCUIntFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CUInt) -> TableGen
addCUIntCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u32_u32 ptr' p1' p2'
addCUIntCULongFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CULong) -> TableGen
addCUIntCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u32_u64 ptr' p1' p2'
addCUIntCUCharFuncs :: TableGen -> (CInt -> CUInt) -> (CInt -> CUChar) -> TableGen
addCUIntCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u32_u8 ptr' p1' p2'
addCULongCBoolFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CBool) -> TableGen
addCULongCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u64_bool ptr' p1' p2'
addCULongCFloatFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CFloat) -> TableGen
addCULongCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u64_f32 ptr' p1' p2'
addCULongCDoubleFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CDouble) -> TableGen
addCULongCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u64_f64 ptr' p1' p2'
addCULongCShortFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CShort) -> TableGen
addCULongCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u64_i16 ptr' p1' p2'
addCULongCIntFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CInt) -> TableGen
addCULongCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u64_i32 ptr' p1' p2'
addCULongCLongFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CLong) -> TableGen
addCULongCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u64_i64 ptr' p1' p2'
addCULongCSCharFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CSChar) -> TableGen
addCULongCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u64_i8 ptr' p1' p2'
addCULongCUShortFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CUShort) -> TableGen
addCULongCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u64_u16 ptr' p1' p2'
addCULongCUIntFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CUInt) -> TableGen
addCULongCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u64_u32 ptr' p1' p2'
addCULongCULongFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CULong) -> TableGen
addCULongCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u64_u64 ptr' p1' p2'
addCULongCUCharFuncs :: TableGen -> (CInt -> CULong) -> (CInt -> CUChar) -> TableGen
addCULongCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u64_u8 ptr' p1' p2'
addCUCharCBoolFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CBool) -> TableGen
addCUCharCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u8_bool ptr' p1' p2'
addCUCharCFloatFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CFloat) -> TableGen
addCUCharCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u8_f32 ptr' p1' p2'
addCUCharCDoubleFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CDouble) -> TableGen
addCUCharCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u8_f64 ptr' p1' p2'
addCUCharCShortFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CShort) -> TableGen
addCUCharCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u8_i16 ptr' p1' p2'
addCUCharCIntFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CInt) -> TableGen
addCUCharCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u8_i32 ptr' p1' p2'
addCUCharCLongFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CLong) -> TableGen
addCUCharCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u8_i64 ptr' p1' p2'
addCUCharCSCharFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CSChar) -> TableGen
addCUCharCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u8_i8 ptr' p1' p2'
addCUCharCUShortFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CUShort) -> TableGen
addCUCharCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u8_u16 ptr' p1' p2'
addCUCharCUIntFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CUInt) -> TableGen
addCUCharCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u8_u32 ptr' p1' p2'
addCUCharCULongFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CULong) -> TableGen
addCUCharCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u8_u64 ptr' p1' p2'
addCUCharCUCharFuncs :: TableGen -> (CInt -> CUChar) -> (CInt -> CUChar) -> TableGen
addCUCharCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u8_u8 ptr' p1' p2'
