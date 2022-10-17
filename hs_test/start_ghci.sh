#!/bin/bash
export LD_PRELOAD_PATH="../target/release"
ghci-9.4.2 ffi.hs -L../target/release -laoc_table