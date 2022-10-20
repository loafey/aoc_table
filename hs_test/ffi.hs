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
main = let
    table = createTable "Howdy from Haskell!"
    table' = (addToTable :: TF CLong CLong) table (sum [0..100000]) 1
    table'' = (addToTable :: TF CDouble CBool) table' 0.2431 1
    in runTable table''

----- auto -----
type TF a b = TableGen -> a -> b -> TableGen
class Tablable a b where addToTable :: TF a b
foreign import ccall "wrapper" wrapCBool :: CBool -> IO (FunPtr CBool)
foreign import ccall "wrapper" wrapCDouble :: CDouble -> IO (FunPtr CDouble)
foreign import ccall "wrapper" wrapCLong :: CLong -> IO (FunPtr CLong)
foreign import ccall "wrapper" wrapCULong :: CULong -> IO (FunPtr CULong)
foreign import ccall "lib.h add_func_bool_bool" rust_add_func_bool_bool :: Ptr Int -> FunPtr CBool -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_f64" rust_add_func_bool_f64 :: Ptr Int -> FunPtr CBool -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_i64" rust_add_func_bool_i64 :: Ptr Int -> FunPtr CBool -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_bool_u64" rust_add_func_bool_u64 :: Ptr Int -> FunPtr CBool -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_bool" rust_add_func_f64_bool :: Ptr Int -> FunPtr CDouble -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_f64" rust_add_func_f64_f64 :: Ptr Int -> FunPtr CDouble -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_i64" rust_add_func_f64_i64 :: Ptr Int -> FunPtr CDouble -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_f64_u64" rust_add_func_f64_u64 :: Ptr Int -> FunPtr CDouble -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_bool" rust_add_func_i64_bool :: Ptr Int -> FunPtr CLong -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_f64" rust_add_func_i64_f64 :: Ptr Int -> FunPtr CLong -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_i64" rust_add_func_i64_i64 :: Ptr Int -> FunPtr CLong -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_i64_u64" rust_add_func_i64_u64 :: Ptr Int -> FunPtr CLong -> FunPtr CULong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_bool" rust_add_func_u64_bool :: Ptr Int -> FunPtr CULong -> FunPtr CBool -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_f64" rust_add_func_u64_f64 :: Ptr Int -> FunPtr CULong -> FunPtr CDouble -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_i64" rust_add_func_u64_i64 :: Ptr Int -> FunPtr CULong -> FunPtr CLong -> IO (Ptr Int)
foreign import ccall "lib.h add_func_u64_u64" rust_add_func_u64_u64 :: Ptr Int -> FunPtr CULong -> FunPtr CULong -> IO (Ptr Int)
instance Tablable CBool CBool where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCBool p1
        let p2' =  unsafePerformIO $ wrapCBool p2
        TableGen $ rust_add_func_bool_bool ptr' p1' p2'
instance Tablable CBool CDouble where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCBool p1
        let p2' =  unsafePerformIO $ wrapCDouble p2
        TableGen $ rust_add_func_bool_f64 ptr' p1' p2'
instance Tablable CBool CLong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCBool p1
        let p2' =  unsafePerformIO $ wrapCLong p2
        TableGen $ rust_add_func_bool_i64 ptr' p1' p2'
instance Tablable CBool CULong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCBool p1
        let p2' =  unsafePerformIO $ wrapCULong p2
        TableGen $ rust_add_func_bool_u64 ptr' p1' p2'
instance Tablable CDouble CBool where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCDouble p1
        let p2' =  unsafePerformIO $ wrapCBool p2
        TableGen $ rust_add_func_f64_bool ptr' p1' p2'
instance Tablable CDouble CDouble where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCDouble p1
        let p2' =  unsafePerformIO $ wrapCDouble p2
        TableGen $ rust_add_func_f64_f64 ptr' p1' p2'
instance Tablable CDouble CLong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCDouble p1
        let p2' =  unsafePerformIO $ wrapCLong p2
        TableGen $ rust_add_func_f64_i64 ptr' p1' p2'
instance Tablable CDouble CULong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCDouble p1
        let p2' =  unsafePerformIO $ wrapCULong p2
        TableGen $ rust_add_func_f64_u64 ptr' p1' p2'
instance Tablable CLong CBool where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCLong p1
        let p2' =  unsafePerformIO $ wrapCBool p2
        TableGen $ rust_add_func_i64_bool ptr' p1' p2'
instance Tablable CLong CDouble where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCLong p1
        let p2' =  unsafePerformIO $ wrapCDouble p2
        TableGen $ rust_add_func_i64_f64 ptr' p1' p2'
instance Tablable CLong CLong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCLong p1
        let p2' =  unsafePerformIO $ wrapCLong p2
        TableGen $ rust_add_func_i64_i64 ptr' p1' p2'
instance Tablable CLong CULong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCLong p1
        let p2' =  unsafePerformIO $ wrapCULong p2
        TableGen $ rust_add_func_i64_u64 ptr' p1' p2'
instance Tablable CULong CBool where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCULong p1
        let p2' =  unsafePerformIO $ wrapCBool p2
        TableGen $ rust_add_func_u64_bool ptr' p1' p2'
instance Tablable CULong CDouble where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCULong p1
        let p2' =  unsafePerformIO $ wrapCDouble p2
        TableGen $ rust_add_func_u64_f64 ptr' p1' p2'
instance Tablable CULong CLong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCULong p1
        let p2' =  unsafePerformIO $ wrapCLong p2
        TableGen $ rust_add_func_u64_i64 ptr' p1' p2'
instance Tablable CULong CULong where
    addToTable (TableGen ptr) p1 p2 = do
        let ptr' = unsafePerformIO ptr
        let p1' =  unsafePerformIO $ wrapCULong p1
        let p2' =  unsafePerformIO $ wrapCULong p2
        TableGen $ rust_add_func_u64_u64 ptr' p1' p2'
