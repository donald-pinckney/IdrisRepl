module Pipes

import Data.Bits

%include C "pipes.c"

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
exec_idris_ide_mode = foreign FFI_C "idris_exec_idris_ide_mode" (IO ())


hexVal : Char -> Maybe Int
hexVal '0' = Just 0
hexVal '1' = Just 1
hexVal '2' = Just 2
hexVal '3' = Just 3
hexVal '4' = Just 4
hexVal '5' = Just 5
hexVal '6' = Just 6
hexVal '7' = Just 7
hexVal '8' = Just 8
hexVal '9' = Just 9
hexVal 'a' = Just 10
hexVal 'b' = Just 11
hexVal 'c' = Just 12
hexVal 'd' = Just 13
hexVal 'e' = Just 14
hexVal 'f' = Just 15
hexVal 'A' = Just 10
hexVal 'B' = Just 11
hexVal 'C' = Just 12
hexVal 'D' = Just 13
hexVal 'E' = Just 14
hexVal 'F' = Just 15
hexVal c = Nothing

valHex : Integer -> Char
valHex 0 = '0'
valHex 1 = '1'
valHex 2 = '2'
valHex 3 = '3'
valHex 4 = '4'
valHex 5 = '5'
valHex 6 = '6'
valHex 7 = '7'
valHex 8 = '8'
valHex 9 = '9'
valHex 10 = 'a'
valHex 11 = 'b'
valHex 12 = 'c'
valHex 13 = 'd'
valHex 14 = 'e'
valHex 15 = 'f'
valHex c = '?'

export
parseHex : List Char -> Int -> Maybe Int
parseHex []        acc = Just acc
parseHex (c :: cs) acc =
  case hexVal c of
      (Just v) => parseHex cs ((acc * 16) + v)
      Nothing => Nothing


printHex : Integer -> String
printHex 0 = ""
printHex x = printHex (x `div` 16) ++ singleton (valHex (x `mod` 16))

export
printHex6 : Integer -> Maybe String
printHex6 x =
    let hex = printHex x in
    if length hex > 6 then
        Nothing
    else
        let toPad = 6 `minus` length hex in
        Just $ pack (take toPad (repeat '0')) ++ hex

export
reportErr : Show a => a -> IO ()
reportErr x = do
    putStrLn $ "Error: " ++ (show x)
    pure ()

export
idris_test : Int -> Int -> Int -> Int -> IO ()
idris_test = foreign FFI_C "idris_test" (Int -> Int -> Int -> Int -> IO ())

export
idris_test_reading : Int -> Int -> IO ()
idris_test_reading = foreign FFI_C "idris_test_reading" (Int -> Int -> IO ())
