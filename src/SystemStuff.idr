module SystemStuff

import Data.Bits
import Config

%include C "system_stuff.c"

export
pipe : IO (Int, Int)
pipe = do
    fd01 <- foreign FFI_C "idris_pipe_64" (IO Bits64)
    let fd1 = prim__truncB64_B32 fd01
    let fd0 = prim__truncB64_B32 $ prim__lshrB64 fd01 32
    -- putStrLn (show fd0)
    -- putStrLn (show fd1)
    pure (prim__zextB32_Int fd0, prim__zextB32_Int fd1)

export
dup2 : Int -> Int -> IO (Maybe Int)
dup2 x y = do
    ret <- foreign FFI_C "idris_dup2" (Int -> Int -> IO Int) x y
    if ret == -1 then
        pure Nothing
    else
        pure $ Just ret


export
close : Int -> IO (Maybe ())
close x = do
    ret <- foreign FFI_C "idris_close" (Int -> IO Int) x
    if ret == 0 then
        pure $ Just ()
    else
        pure Nothing

export
fork_proc : IO Int
fork_proc = foreign FFI_C "idris_fork_proc" (IO Int)


do_fdopen : Int -> String -> IO Ptr
do_fdopen fd m = foreign FFI_C "fileDescOpen" (Int -> String -> IO Ptr) fd m

export
fdopen : Int -> String -> IO (Either FileError File)
fdopen fd m = do
    h <- do_fdopen fd m
    if !(nullPtr h) then do
        err <- getFileError
        pure (Left err)
    else
        pure (Right (FHandle h))

export
setlinebuf : File -> IO ()
setlinebuf (FHandle p) = foreign FFI_C "idris_setlinebuf" (Ptr -> IO ()) p


export
exec_idris_ide_mode : IO ()
exec_idris_ide_mode = foreign FFI_C "idris_exec_idris_ide_mode" (String -> IO ()) IDRIS_NAME
