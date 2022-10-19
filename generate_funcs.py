#!/bin/python3
import os


def replaceRustType(type):
    match type:
        case "bool":
            return "CBool"
        case "u8":
            return "CUChar"
        case "i8":
            return "CSChar"
        case "u16":
            return "CUShort"
        case "i16":
            return "CShort"
        case "u32":
            return "CUInt"
        case "i32":
            return "CInt"
        case "u64":
            return "CULong"
        case "i64":
            return "CLong"
        case "f32":
            return "CFloat"
        case "f64":
            return "CDouble"
        case other:
            return type


def getTypes(function):
    type1 = replaceRustType(function.split('_')[2])
    type2 = replaceRustType(function.split('_')[3])
    return (type1, type2)


def createImportFunction(function):
    type1 = replaceRustType(function.split('_')[2])
    type2 = replaceRustType(function.split('_')[3])
    fixed = f"""foreign import ccall "lib.h {function}" rust_{function} :: Ptr Int -> FunPtr {type1} -> FunPtr {type2} -> IO (Ptr Int)"""
    return fixed


def createTypeWrapper(type):
    return f"""foreign import ccall "wrapper" wrap{type} :: {type} -> IO (FunPtr {type})"""


def createAddFuncFunction(function):
    type1 = replaceRustType(function.split('_')[2])
    type2 = replaceRustType(function.split('_')[3])
    finished = f"""add{type1}{type2}Funcs :: TableGen -> {type1} -> {type2} -> TableGen
add{type1}{type2}Funcs (TableGen ptr) p1 p2 = do
    let ptr' = unsafePerformIO ptr
    let p1' =  unsafePerformIO $ wrap{type1} p1
    let p2' =  unsafePerformIO $ wrap{type2} p2
    TableGen $ rust_{function} ptr' p1' p2'"""
    return finished


os.system("cargo build --release")
nmOutput = os.popen(
    "nm target/release/libaoc_table.so | grep add_func_ | awk '{ print $3 }'")
lines = list(map(
    lambda s: s[0: len(s) - 1], nmOutput.readlines()))

types = list(dict.fromkeys(
    [item for t in list(map(getTypes, lines)) for item in t]))

wrappers = list(map(createTypeWrapper, types))
c_imports = list(map(createImportFunction, lines))
functions = list(map(createAddFuncFunction, lines))

for w in wrappers:
    print(w)

for c_import in c_imports:
    print(c_import)

for function in functions:
    print(function)

# what we need, wrappers, c_imports, functions
