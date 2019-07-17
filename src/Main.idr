module Main

import GenericRepl
import Shared
import System
import Data.Bits
import SystemStuff
import IdeProtocol
import Repl


STDIN_FILENO : Int
STDIN_FILENO = 0

STDOUT_FILENO : Int
STDOUT_FILENO = 1

fork_idris_proc : IO (File, File)
fork_idris_proc = do
    (send_read, send_write) <- pipe
    (recv_read, recv_write) <- pipe
    p <- fork_proc
    case p of
        0 => do
            close send_write
            dup2 send_read STDIN_FILENO
            close send_read

            close recv_read
            dup2 recv_write STDOUT_FILENO
            close recv_write

            exec_idris_ide_mode

            believe_me ()
        _ => do
            close recv_write
            close send_read

            Right read_f <- fdopen recv_read "r"
                | Left err => fatalError err
            Right write_f <- fdopen send_write "w"
                | Left err => fatalError err

            setlinebuf read_f
            setlinebuf write_f

            pure (read_f, write_f)

export
main : IO ()
main = do
    (read_f, write_f) <- fork_idris_proc

    Right recBuf <- readIdeLine read_f
        | Left err => fatalError err

    idrisRepl read_f write_f
