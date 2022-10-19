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
    let table' = addCIntCBoolFuncs table (sum [0..100]) 1
    let table'' = addCFloatCIntFuncs table' (0.2431) 1
    runTable table''

----- auto -----
foreign import ccall "wrapper" wrapCBool :: CBool -> IO (FunPtr CBool)
foreign import ccall "wrapper" wrapCFloat :: CFloat -> IO (FunPtr CFloat)
foreign import ccall "wrapper" wrapCDouble :: CDouble -> IO (FunPtr CDouble)
foreign import ccall "wrapper" wrapCShort :: CShort -> IO (FunPtr CShort)
foreign import ccall "wrapper" wrapCInt :: CInt -> IO (FunPtr CInt)
foreign import ccall "wrapper" wrapCLong :: CLong -> IO (FunPtr CLong)
foreign import ccall "wrapper" wrapCSChar :: CSChar -> IO (FunPtr CSChar)
foreign import ccall "wrapper" wrapCUShort :: CUShort -> IO (FunPtr CUShort)
foreign import ccall "wrapper" wrapCUInt :: CUInt -> IO (FunPtr CUInt)
foreign import ccall "wrapper" wrapCULong :: CULong -> IO (FunPtr CULong)
foreign import ccall "wrapper" wrapCUChar :: CUChar -> IO (FunPtr CUChar)
foreign import ccall "lib.h add_func_bool_bool" rust_add_func_bool_bool :: Ptr Int -> FunPtr CBool -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_f32" rust_add_func_bool_f32 :: Ptr Int -> FunPtr CBool -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_f64" rust_add_func_bool_f64 :: Ptr Int -> FunPtr CBool -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i16" rust_add_func_bool_i16 :: Ptr Int -> FunPtr CBool -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i32" rust_add_func_bool_i32 :: Ptr Int -> FunPtr CBool -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i64" rust_add_func_bool_i64 :: Ptr Int -> FunPtr CBool -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i8" rust_add_func_bool_i8 :: Ptr Int -> FunPtr CBool -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u16" rust_add_func_bool_u16 :: Ptr Int -> FunPtr CBool -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u32" rust_add_func_bool_u32 :: Ptr Int -> FunPtr CBool -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u64" rust_add_func_bool_u64 :: Ptr Int -> FunPtr CBool -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u8" rust_add_func_bool_u8 :: Ptr Int -> FunPtr CBool -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_bool" rust_add_func_f32_bool :: Ptr Int -> FunPtr CFloat -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_f32" rust_add_func_f32_f32 :: Ptr Int -> FunPtr CFloat -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_f64" rust_add_func_f32_f64 :: Ptr Int -> FunPtr CFloat -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i16" rust_add_func_f32_i16 :: Ptr Int -> FunPtr CFloat -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i32" rust_add_func_f32_i32 :: Ptr Int -> FunPtr CFloat -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i64" rust_add_func_f32_i64 :: Ptr Int -> FunPtr CFloat -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_i8" rust_add_func_f32_i8 :: Ptr Int -> FunPtr CFloat -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u16" rust_add_func_f32_u16 :: Ptr Int -> FunPtr CFloat -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u32" rust_add_func_f32_u32 :: Ptr Int -> FunPtr CFloat -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u64" rust_add_func_f32_u64 :: Ptr Int -> FunPtr CFloat -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f32_u8" rust_add_func_f32_u8 :: Ptr Int -> FunPtr CFloat -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_bool" rust_add_func_f64_bool :: Ptr Int -> FunPtr CDouble -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_f32" rust_add_func_f64_f32 :: Ptr Int -> FunPtr CDouble -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_f64" rust_add_func_f64_f64 :: Ptr Int -> FunPtr CDouble -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i16" rust_add_func_f64_i16 :: Ptr Int -> FunPtr CDouble -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i32" rust_add_func_f64_i32 :: Ptr Int -> FunPtr CDouble -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i64" rust_add_func_f64_i64 :: Ptr Int -> FunPtr CDouble -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i8" rust_add_func_f64_i8 :: Ptr Int -> FunPtr CDouble -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u16" rust_add_func_f64_u16 :: Ptr Int -> FunPtr CDouble -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u32" rust_add_func_f64_u32 :: Ptr Int -> FunPtr CDouble -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u64" rust_add_func_f64_u64 :: Ptr Int -> FunPtr CDouble -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u8" rust_add_func_f64_u8 :: Ptr Int -> FunPtr CDouble -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_bool" rust_add_func_i16_bool :: Ptr Int -> FunPtr CShort -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_f32" rust_add_func_i16_f32 :: Ptr Int -> FunPtr CShort -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_f64" rust_add_func_i16_f64 :: Ptr Int -> FunPtr CShort -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i16" rust_add_func_i16_i16 :: Ptr Int -> FunPtr CShort -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i32" rust_add_func_i16_i32 :: Ptr Int -> FunPtr CShort -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i64" rust_add_func_i16_i64 :: Ptr Int -> FunPtr CShort -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_i8" rust_add_func_i16_i8 :: Ptr Int -> FunPtr CShort -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u16" rust_add_func_i16_u16 :: Ptr Int -> FunPtr CShort -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u32" rust_add_func_i16_u32 :: Ptr Int -> FunPtr CShort -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u64" rust_add_func_i16_u64 :: Ptr Int -> FunPtr CShort -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i16_u8" rust_add_func_i16_u8 :: Ptr Int -> FunPtr CShort -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_bool" rust_add_func_i32_bool :: Ptr Int -> FunPtr CInt -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_f32" rust_add_func_i32_f32 :: Ptr Int -> FunPtr CInt -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_f64" rust_add_func_i32_f64 :: Ptr Int -> FunPtr CInt -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i16" rust_add_func_i32_i16 :: Ptr Int -> FunPtr CInt -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i32" rust_add_func_i32_i32 :: Ptr Int -> FunPtr CInt -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i64" rust_add_func_i32_i64 :: Ptr Int -> FunPtr CInt -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_i8" rust_add_func_i32_i8 :: Ptr Int -> FunPtr CInt -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u16" rust_add_func_i32_u16 :: Ptr Int -> FunPtr CInt -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u32" rust_add_func_i32_u32 :: Ptr Int -> FunPtr CInt -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u64" rust_add_func_i32_u64 :: Ptr Int -> FunPtr CInt -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i32_u8" rust_add_func_i32_u8 :: Ptr Int -> FunPtr CInt -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_bool" rust_add_func_i64_bool :: Ptr Int -> FunPtr CLong -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_f32" rust_add_func_i64_f32 :: Ptr Int -> FunPtr CLong -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_f64" rust_add_func_i64_f64 :: Ptr Int -> FunPtr CLong -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i16" rust_add_func_i64_i16 :: Ptr Int -> FunPtr CLong -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i32" rust_add_func_i64_i32 :: Ptr Int -> FunPtr CLong -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i64" rust_add_func_i64_i64 :: Ptr Int -> FunPtr CLong -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i8" rust_add_func_i64_i8 :: Ptr Int -> FunPtr CLong -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u16" rust_add_func_i64_u16 :: Ptr Int -> FunPtr CLong -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u32" rust_add_func_i64_u32 :: Ptr Int -> FunPtr CLong -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u64" rust_add_func_i64_u64 :: Ptr Int -> FunPtr CLong -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u8" rust_add_func_i64_u8 :: Ptr Int -> FunPtr CLong -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_bool" rust_add_func_i8_bool :: Ptr Int -> FunPtr CSChar -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_f32" rust_add_func_i8_f32 :: Ptr Int -> FunPtr CSChar -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_f64" rust_add_func_i8_f64 :: Ptr Int -> FunPtr CSChar -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i16" rust_add_func_i8_i16 :: Ptr Int -> FunPtr CSChar -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i32" rust_add_func_i8_i32 :: Ptr Int -> FunPtr CSChar -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i64" rust_add_func_i8_i64 :: Ptr Int -> FunPtr CSChar -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_i8" rust_add_func_i8_i8 :: Ptr Int -> FunPtr CSChar -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u16" rust_add_func_i8_u16 :: Ptr Int -> FunPtr CSChar -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u32" rust_add_func_i8_u32 :: Ptr Int -> FunPtr CSChar -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u64" rust_add_func_i8_u64 :: Ptr Int -> FunPtr CSChar -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i8_u8" rust_add_func_i8_u8 :: Ptr Int -> FunPtr CSChar -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_bool" rust_add_func_u16_bool :: Ptr Int -> FunPtr CUShort -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_f32" rust_add_func_u16_f32 :: Ptr Int -> FunPtr CUShort -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_f64" rust_add_func_u16_f64 :: Ptr Int -> FunPtr CUShort -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i16" rust_add_func_u16_i16 :: Ptr Int -> FunPtr CUShort -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i32" rust_add_func_u16_i32 :: Ptr Int -> FunPtr CUShort -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i64" rust_add_func_u16_i64 :: Ptr Int -> FunPtr CUShort -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_i8" rust_add_func_u16_i8 :: Ptr Int -> FunPtr CUShort -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u16" rust_add_func_u16_u16 :: Ptr Int -> FunPtr CUShort -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u32" rust_add_func_u16_u32 :: Ptr Int -> FunPtr CUShort -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u64" rust_add_func_u16_u64 :: Ptr Int -> FunPtr CUShort -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u16_u8" rust_add_func_u16_u8 :: Ptr Int -> FunPtr CUShort -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_bool" rust_add_func_u32_bool :: Ptr Int -> FunPtr CUInt -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_f32" rust_add_func_u32_f32 :: Ptr Int -> FunPtr CUInt -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_f64" rust_add_func_u32_f64 :: Ptr Int -> FunPtr CUInt -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i16" rust_add_func_u32_i16 :: Ptr Int -> FunPtr CUInt -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i32" rust_add_func_u32_i32 :: Ptr Int -> FunPtr CUInt -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i64" rust_add_func_u32_i64 :: Ptr Int -> FunPtr CUInt -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_i8" rust_add_func_u32_i8 :: Ptr Int -> FunPtr CUInt -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u16" rust_add_func_u32_u16 :: Ptr Int -> FunPtr CUInt -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u32" rust_add_func_u32_u32 :: Ptr Int -> FunPtr CUInt -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u64" rust_add_func_u32_u64 :: Ptr Int -> FunPtr CUInt -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u32_u8" rust_add_func_u32_u8 :: Ptr Int -> FunPtr CUInt -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_bool" rust_add_func_u64_bool :: Ptr Int -> FunPtr CULong -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_f32" rust_add_func_u64_f32 :: Ptr Int -> FunPtr CULong -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_f64" rust_add_func_u64_f64 :: Ptr Int -> FunPtr CULong -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i16" rust_add_func_u64_i16 :: Ptr Int -> FunPtr CULong -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i32" rust_add_func_u64_i32 :: Ptr Int -> FunPtr CULong -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i64" rust_add_func_u64_i64 :: Ptr Int -> FunPtr CULong -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i8" rust_add_func_u64_i8 :: Ptr Int -> FunPtr CULong -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u16" rust_add_func_u64_u16 :: Ptr Int -> FunPtr CULong -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u32" rust_add_func_u64_u32 :: Ptr Int -> FunPtr CULong -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u64" rust_add_func_u64_u64 :: Ptr Int -> FunPtr CULong -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u8" rust_add_func_u64_u8 :: Ptr Int -> FunPtr CULong -> FunPtr CUChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_bool" rust_add_func_u8_bool :: Ptr Int -> FunPtr CUChar -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_f32" rust_add_func_u8_f32 :: Ptr Int -> FunPtr CUChar -> FunPtr CFloat -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_f64" rust_add_func_u8_f64 :: Ptr Int -> FunPtr CUChar -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i16" rust_add_func_u8_i16 :: Ptr Int -> FunPtr CUChar -> FunPtr CShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i32" rust_add_func_u8_i32 :: Ptr Int -> FunPtr CUChar -> FunPtr CInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i64" rust_add_func_u8_i64 :: Ptr Int -> FunPtr CUChar -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_i8" rust_add_func_u8_i8 :: Ptr Int -> FunPtr CUChar -> FunPtr CSChar -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u16" rust_add_func_u8_u16 :: Ptr Int -> FunPtr CUChar -> FunPtr CUShort -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u32" rust_add_func_u8_u32 :: Ptr Int -> FunPtr CUChar -> FunPtr CUInt -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u64" rust_add_func_u8_u64 :: Ptr Int -> FunPtr CUChar -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u8_u8" rust_add_func_u8_u8 :: Ptr Int -> FunPtr CUChar -> FunPtr CUChar -> IO (Ptr Int)
addCBoolCBoolFuncs :: TableGen -> CBool -> CBool -> TableGen
addCBoolCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_bool_bool ptr' p1' p2'
addCBoolCFloatFuncs :: TableGen -> CBool -> CFloat -> TableGen
addCBoolCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_bool_f32 ptr' p1' p2'
addCBoolCDoubleFuncs :: TableGen -> CBool -> CDouble -> TableGen
addCBoolCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_bool_f64 ptr' p1' p2'
addCBoolCShortFuncs :: TableGen -> CBool -> CShort -> TableGen
addCBoolCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_bool_i16 ptr' p1' p2'
addCBoolCIntFuncs :: TableGen -> CBool -> CInt -> TableGen
addCBoolCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_bool_i32 ptr' p1' p2'
addCBoolCLongFuncs :: TableGen -> CBool -> CLong -> TableGen
addCBoolCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_bool_i64 ptr' p1' p2'
addCBoolCSCharFuncs :: TableGen -> CBool -> CSChar -> TableGen
addCBoolCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_bool_i8 ptr' p1' p2'
addCBoolCUShortFuncs :: TableGen -> CBool -> CUShort -> TableGen
addCBoolCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_bool_u16 ptr' p1' p2'
addCBoolCUIntFuncs :: TableGen -> CBool -> CUInt -> TableGen
addCBoolCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_bool_u32 ptr' p1' p2'
addCBoolCULongFuncs :: TableGen -> CBool -> CULong -> TableGen
addCBoolCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_bool_u64 ptr' p1' p2'
addCBoolCUCharFuncs :: TableGen -> CBool -> CUChar -> TableGen
addCBoolCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCBool p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_bool_u8 ptr' p1' p2'
addCFloatCBoolFuncs :: TableGen -> CFloat -> CBool -> TableGen
addCFloatCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_f32_bool ptr' p1' p2'
addCFloatCFloatFuncs :: TableGen -> CFloat -> CFloat -> TableGen
addCFloatCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_f32_f32 ptr' p1' p2'
addCFloatCDoubleFuncs :: TableGen -> CFloat -> CDouble -> TableGen
addCFloatCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_f32_f64 ptr' p1' p2'
addCFloatCShortFuncs :: TableGen -> CFloat -> CShort -> TableGen
addCFloatCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_f32_i16 ptr' p1' p2'
addCFloatCIntFuncs :: TableGen -> CFloat -> CInt -> TableGen
addCFloatCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_f32_i32 ptr' p1' p2'
addCFloatCLongFuncs :: TableGen -> CFloat -> CLong -> TableGen
addCFloatCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_f32_i64 ptr' p1' p2'
addCFloatCSCharFuncs :: TableGen -> CFloat -> CSChar -> TableGen
addCFloatCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_f32_i8 ptr' p1' p2'
addCFloatCUShortFuncs :: TableGen -> CFloat -> CUShort -> TableGen
addCFloatCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_f32_u16 ptr' p1' p2'
addCFloatCUIntFuncs :: TableGen -> CFloat -> CUInt -> TableGen
addCFloatCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_f32_u32 ptr' p1' p2'
addCFloatCULongFuncs :: TableGen -> CFloat -> CULong -> TableGen
addCFloatCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_f32_u64 ptr' p1' p2'
addCFloatCUCharFuncs :: TableGen -> CFloat -> CUChar -> TableGen
addCFloatCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCFloat p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_f32_u8 ptr' p1' p2'
addCDoubleCBoolFuncs :: TableGen -> CDouble -> CBool -> TableGen
addCDoubleCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_f64_bool ptr' p1' p2'
addCDoubleCFloatFuncs :: TableGen -> CDouble -> CFloat -> TableGen
addCDoubleCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_f64_f32 ptr' p1' p2'
addCDoubleCDoubleFuncs :: TableGen -> CDouble -> CDouble -> TableGen
addCDoubleCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_f64_f64 ptr' p1' p2'
addCDoubleCShortFuncs :: TableGen -> CDouble -> CShort -> TableGen
addCDoubleCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_f64_i16 ptr' p1' p2'
addCDoubleCIntFuncs :: TableGen -> CDouble -> CInt -> TableGen
addCDoubleCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_f64_i32 ptr' p1' p2'
addCDoubleCLongFuncs :: TableGen -> CDouble -> CLong -> TableGen
addCDoubleCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_f64_i64 ptr' p1' p2'
addCDoubleCSCharFuncs :: TableGen -> CDouble -> CSChar -> TableGen
addCDoubleCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_f64_i8 ptr' p1' p2'
addCDoubleCUShortFuncs :: TableGen -> CDouble -> CUShort -> TableGen
addCDoubleCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_f64_u16 ptr' p1' p2'
addCDoubleCUIntFuncs :: TableGen -> CDouble -> CUInt -> TableGen
addCDoubleCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_f64_u32 ptr' p1' p2'
addCDoubleCULongFuncs :: TableGen -> CDouble -> CULong -> TableGen
addCDoubleCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_f64_u64 ptr' p1' p2'
addCDoubleCUCharFuncs :: TableGen -> CDouble -> CUChar -> TableGen
addCDoubleCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCDouble p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_f64_u8 ptr' p1' p2'
addCShortCBoolFuncs :: TableGen -> CShort -> CBool -> TableGen
addCShortCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i16_bool ptr' p1' p2'
addCShortCFloatFuncs :: TableGen -> CShort -> CFloat -> TableGen
addCShortCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i16_f32 ptr' p1' p2'
addCShortCDoubleFuncs :: TableGen -> CShort -> CDouble -> TableGen
addCShortCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i16_f64 ptr' p1' p2'
addCShortCShortFuncs :: TableGen -> CShort -> CShort -> TableGen
addCShortCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i16_i16 ptr' p1' p2'
addCShortCIntFuncs :: TableGen -> CShort -> CInt -> TableGen
addCShortCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i16_i32 ptr' p1' p2'
addCShortCLongFuncs :: TableGen -> CShort -> CLong -> TableGen
addCShortCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i16_i64 ptr' p1' p2'
addCShortCSCharFuncs :: TableGen -> CShort -> CSChar -> TableGen
addCShortCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i16_i8 ptr' p1' p2'
addCShortCUShortFuncs :: TableGen -> CShort -> CUShort -> TableGen
addCShortCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i16_u16 ptr' p1' p2'
addCShortCUIntFuncs :: TableGen -> CShort -> CUInt -> TableGen
addCShortCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i16_u32 ptr' p1' p2'
addCShortCULongFuncs :: TableGen -> CShort -> CULong -> TableGen
addCShortCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i16_u64 ptr' p1' p2'
addCShortCUCharFuncs :: TableGen -> CShort -> CUChar -> TableGen
addCShortCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCShort p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i16_u8 ptr' p1' p2'
addCIntCBoolFuncs :: TableGen -> CInt -> CBool -> TableGen
addCIntCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i32_bool ptr' p1' p2'
addCIntCFloatFuncs :: TableGen -> CInt -> CFloat -> TableGen
addCIntCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i32_f32 ptr' p1' p2'
addCIntCDoubleFuncs :: TableGen -> CInt -> CDouble -> TableGen
addCIntCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i32_f64 ptr' p1' p2'
addCIntCShortFuncs :: TableGen -> CInt -> CShort -> TableGen
addCIntCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i32_i16 ptr' p1' p2'
addCIntCIntFuncs :: TableGen -> CInt -> CInt -> TableGen
addCIntCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i32_i32 ptr' p1' p2'
addCIntCLongFuncs :: TableGen -> CInt -> CLong -> TableGen
addCIntCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i32_i64 ptr' p1' p2'
addCIntCSCharFuncs :: TableGen -> CInt -> CSChar -> TableGen
addCIntCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i32_i8 ptr' p1' p2'
addCIntCUShortFuncs :: TableGen -> CInt -> CUShort -> TableGen
addCIntCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i32_u16 ptr' p1' p2'
addCIntCUIntFuncs :: TableGen -> CInt -> CUInt -> TableGen
addCIntCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i32_u32 ptr' p1' p2'
addCIntCULongFuncs :: TableGen -> CInt -> CULong -> TableGen
addCIntCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i32_u64 ptr' p1' p2'
addCIntCUCharFuncs :: TableGen -> CInt -> CUChar -> TableGen
addCIntCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCInt p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i32_u8 ptr' p1' p2'
addCLongCBoolFuncs :: TableGen -> CLong -> CBool -> TableGen
addCLongCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i64_bool ptr' p1' p2'
addCLongCFloatFuncs :: TableGen -> CLong -> CFloat -> TableGen
addCLongCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i64_f32 ptr' p1' p2'
addCLongCDoubleFuncs :: TableGen -> CLong -> CDouble -> TableGen
addCLongCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i64_f64 ptr' p1' p2'
addCLongCShortFuncs :: TableGen -> CLong -> CShort -> TableGen
addCLongCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i64_i16 ptr' p1' p2'
addCLongCIntFuncs :: TableGen -> CLong -> CInt -> TableGen
addCLongCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i64_i32 ptr' p1' p2'
addCLongCLongFuncs :: TableGen -> CLong -> CLong -> TableGen
addCLongCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i64_i64 ptr' p1' p2'
addCLongCSCharFuncs :: TableGen -> CLong -> CSChar -> TableGen
addCLongCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i64_i8 ptr' p1' p2'
addCLongCUShortFuncs :: TableGen -> CLong -> CUShort -> TableGen
addCLongCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i64_u16 ptr' p1' p2'
addCLongCUIntFuncs :: TableGen -> CLong -> CUInt -> TableGen
addCLongCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i64_u32 ptr' p1' p2'
addCLongCULongFuncs :: TableGen -> CLong -> CULong -> TableGen
addCLongCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i64_u64 ptr' p1' p2'
addCLongCUCharFuncs :: TableGen -> CLong -> CUChar -> TableGen
addCLongCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCLong p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i64_u8 ptr' p1' p2'
addCSCharCBoolFuncs :: TableGen -> CSChar -> CBool -> TableGen
addCSCharCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_i8_bool ptr' p1' p2'
addCSCharCFloatFuncs :: TableGen -> CSChar -> CFloat -> TableGen
addCSCharCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_i8_f32 ptr' p1' p2'
addCSCharCDoubleFuncs :: TableGen -> CSChar -> CDouble -> TableGen
addCSCharCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_i8_f64 ptr' p1' p2'
addCSCharCShortFuncs :: TableGen -> CSChar -> CShort -> TableGen
addCSCharCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_i8_i16 ptr' p1' p2'
addCSCharCIntFuncs :: TableGen -> CSChar -> CInt -> TableGen
addCSCharCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_i8_i32 ptr' p1' p2'
addCSCharCLongFuncs :: TableGen -> CSChar -> CLong -> TableGen
addCSCharCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_i8_i64 ptr' p1' p2'
addCSCharCSCharFuncs :: TableGen -> CSChar -> CSChar -> TableGen
addCSCharCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_i8_i8 ptr' p1' p2'
addCSCharCUShortFuncs :: TableGen -> CSChar -> CUShort -> TableGen
addCSCharCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_i8_u16 ptr' p1' p2'
addCSCharCUIntFuncs :: TableGen -> CSChar -> CUInt -> TableGen
addCSCharCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_i8_u32 ptr' p1' p2'
addCSCharCULongFuncs :: TableGen -> CSChar -> CULong -> TableGen
addCSCharCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_i8_u64 ptr' p1' p2'
addCSCharCUCharFuncs :: TableGen -> CSChar -> CUChar -> TableGen
addCSCharCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCSChar p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_i8_u8 ptr' p1' p2'
addCUShortCBoolFuncs :: TableGen -> CUShort -> CBool -> TableGen
addCUShortCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u16_bool ptr' p1' p2'
addCUShortCFloatFuncs :: TableGen -> CUShort -> CFloat -> TableGen
addCUShortCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u16_f32 ptr' p1' p2'
addCUShortCDoubleFuncs :: TableGen -> CUShort -> CDouble -> TableGen
addCUShortCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u16_f64 ptr' p1' p2'
addCUShortCShortFuncs :: TableGen -> CUShort -> CShort -> TableGen
addCUShortCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u16_i16 ptr' p1' p2'
addCUShortCIntFuncs :: TableGen -> CUShort -> CInt -> TableGen
addCUShortCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u16_i32 ptr' p1' p2'
addCUShortCLongFuncs :: TableGen -> CUShort -> CLong -> TableGen
addCUShortCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u16_i64 ptr' p1' p2'
addCUShortCSCharFuncs :: TableGen -> CUShort -> CSChar -> TableGen
addCUShortCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u16_i8 ptr' p1' p2'
addCUShortCUShortFuncs :: TableGen -> CUShort -> CUShort -> TableGen
addCUShortCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u16_u16 ptr' p1' p2'
addCUShortCUIntFuncs :: TableGen -> CUShort -> CUInt -> TableGen
addCUShortCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u16_u32 ptr' p1' p2'
addCUShortCULongFuncs :: TableGen -> CUShort -> CULong -> TableGen
addCUShortCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u16_u64 ptr' p1' p2'
addCUShortCUCharFuncs :: TableGen -> CUShort -> CUChar -> TableGen
addCUShortCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUShort p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u16_u8 ptr' p1' p2'
addCUIntCBoolFuncs :: TableGen -> CUInt -> CBool -> TableGen
addCUIntCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u32_bool ptr' p1' p2'
addCUIntCFloatFuncs :: TableGen -> CUInt -> CFloat -> TableGen
addCUIntCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u32_f32 ptr' p1' p2'
addCUIntCDoubleFuncs :: TableGen -> CUInt -> CDouble -> TableGen
addCUIntCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u32_f64 ptr' p1' p2'
addCUIntCShortFuncs :: TableGen -> CUInt -> CShort -> TableGen
addCUIntCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u32_i16 ptr' p1' p2'
addCUIntCIntFuncs :: TableGen -> CUInt -> CInt -> TableGen
addCUIntCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u32_i32 ptr' p1' p2'
addCUIntCLongFuncs :: TableGen -> CUInt -> CLong -> TableGen
addCUIntCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u32_i64 ptr' p1' p2'
addCUIntCSCharFuncs :: TableGen -> CUInt -> CSChar -> TableGen
addCUIntCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u32_i8 ptr' p1' p2'
addCUIntCUShortFuncs :: TableGen -> CUInt -> CUShort -> TableGen
addCUIntCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u32_u16 ptr' p1' p2'
addCUIntCUIntFuncs :: TableGen -> CUInt -> CUInt -> TableGen
addCUIntCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u32_u32 ptr' p1' p2'
addCUIntCULongFuncs :: TableGen -> CUInt -> CULong -> TableGen
addCUIntCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u32_u64 ptr' p1' p2'
addCUIntCUCharFuncs :: TableGen -> CUInt -> CUChar -> TableGen
addCUIntCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUInt p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u32_u8 ptr' p1' p2'
addCULongCBoolFuncs :: TableGen -> CULong -> CBool -> TableGen
addCULongCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u64_bool ptr' p1' p2'
addCULongCFloatFuncs :: TableGen -> CULong -> CFloat -> TableGen
addCULongCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u64_f32 ptr' p1' p2'
addCULongCDoubleFuncs :: TableGen -> CULong -> CDouble -> TableGen
addCULongCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u64_f64 ptr' p1' p2'
addCULongCShortFuncs :: TableGen -> CULong -> CShort -> TableGen
addCULongCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u64_i16 ptr' p1' p2'
addCULongCIntFuncs :: TableGen -> CULong -> CInt -> TableGen
addCULongCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u64_i32 ptr' p1' p2'
addCULongCLongFuncs :: TableGen -> CULong -> CLong -> TableGen
addCULongCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u64_i64 ptr' p1' p2'
addCULongCSCharFuncs :: TableGen -> CULong -> CSChar -> TableGen
addCULongCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u64_i8 ptr' p1' p2'
addCULongCUShortFuncs :: TableGen -> CULong -> CUShort -> TableGen
addCULongCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u64_u16 ptr' p1' p2'
addCULongCUIntFuncs :: TableGen -> CULong -> CUInt -> TableGen
addCULongCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u64_u32 ptr' p1' p2'
addCULongCULongFuncs :: TableGen -> CULong -> CULong -> TableGen
addCULongCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u64_u64 ptr' p1' p2'
addCULongCUCharFuncs :: TableGen -> CULong -> CUChar -> TableGen
addCULongCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCULong p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u64_u8 ptr' p1' p2'
addCUCharCBoolFuncs :: TableGen -> CUChar -> CBool -> TableGen
addCUCharCBoolFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCBool p2
    TableGen $ rust_add_func_u8_bool ptr' p1' p2'
addCUCharCFloatFuncs :: TableGen -> CUChar -> CFloat -> TableGen
addCUCharCFloatFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCFloat p2
    TableGen $ rust_add_func_u8_f32 ptr' p1' p2'
addCUCharCDoubleFuncs :: TableGen -> CUChar -> CDouble -> TableGen
addCUCharCDoubleFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCDouble p2
    TableGen $ rust_add_func_u8_f64 ptr' p1' p2'
addCUCharCShortFuncs :: TableGen -> CUChar -> CShort -> TableGen
addCUCharCShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCShort p2
    TableGen $ rust_add_func_u8_i16 ptr' p1' p2'
addCUCharCIntFuncs :: TableGen -> CUChar -> CInt -> TableGen
addCUCharCIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCInt p2
    TableGen $ rust_add_func_u8_i32 ptr' p1' p2'
addCUCharCLongFuncs :: TableGen -> CUChar -> CLong -> TableGen
addCUCharCLongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCLong p2
    TableGen $ rust_add_func_u8_i64 ptr' p1' p2'
addCUCharCSCharFuncs :: TableGen -> CUChar -> CSChar -> TableGen
addCUCharCSCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCSChar p2
    TableGen $ rust_add_func_u8_i8 ptr' p1' p2'
addCUCharCUShortFuncs :: TableGen -> CUChar -> CUShort -> TableGen
addCUCharCUShortFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCUShort p2
    TableGen $ rust_add_func_u8_u16 ptr' p1' p2'
addCUCharCUIntFuncs :: TableGen -> CUChar -> CUInt -> TableGen
addCUCharCUIntFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCUInt p2
    TableGen $ rust_add_func_u8_u32 ptr' p1' p2'
addCUCharCULongFuncs :: TableGen -> CUChar -> CULong -> TableGen
addCUCharCULongFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCULong p2
    TableGen $ rust_add_func_u8_u64 ptr' p1' p2'
addCUCharCUCharFuncs :: TableGen -> CUChar -> CUChar -> TableGen
addCUCharCUCharFuncs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrapCUChar p1
    let p2' =  unsafePerformIO $ wrapCUChar p2
    TableGen $ rust_add_func_u8_u8 ptr' p1' p2'
