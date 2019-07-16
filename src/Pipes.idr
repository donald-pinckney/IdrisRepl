module Pipes

import Data.Bits

%include C "pipes.c"

export
pipe_safe : IO (Int, Int)
pipe_safe = do
    fd01 <- foreign FFI_C "idris_pipe_64" (IO Bits64)
    let fd1 = prim__truncB64_B32 fd01
    let fd0 = prim__truncB64_B32 $ prim__lshrB64 fd01 32
    -- putStrLn (show fd0)
    -- putStrLn (show fd1)
    pure (prim__zextB32_Int fd0, prim__zextB32_Int fd1)

export
dup2 : Int -> Int -> IO (Maybe Int)
dup2 x y = do
    ret <- foreign FFI_C "dup2" (Int -> Int -> IO Int) x y
    if ret == -1 then
        pure Nothing
    else
        pure $ Just ret


export
close : Int -> IO (Maybe ())
close x = do
    ret <- foreign FFI_C "close" (Int -> IO Int) x
    if ret == 0 then
        pure $ Just ()
    else
        pure Nothing
