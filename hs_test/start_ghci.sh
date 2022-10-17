#!/bin/bash

# LD_PRELOAD_PATH="/home/samuel/Git/aoc_table/target/release" 
# -L/home/samuel/Git/aoc_table/target/release -laoc_table 
cargo build --release
LD_LIBRARY_PATH="../target/release" ghci-9.4.2 ffi.hs -L../target/release  -laoc_table
