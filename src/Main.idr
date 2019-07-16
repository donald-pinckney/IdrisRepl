module Main

import GenericRepl
import Shared
import System
import Data.Bits
import Pipes

Command_echo : String -> ReplCommand
Command_echo str = \_ => do
    putStrLn "ECHO:"
    pure $ Right $ Just str

SupportedCommands : List CommandBuilder
SupportedCommands = [
    MkCommandBuilder [['e'],['e','c','h','o']]
        "<term>"
        "Performs beta multistep on <term> until it is in BNF."
        (pure . Command_echo . pack)
]

initializeRepl : IO ()
initializeRepl = do
    (pr, pw) <- pipe_safe
    putStrLn (show pr)
    putStrLn (show pw)


    -- putStrLn "cats"
    -- fork $ do
    -- -- n <- system "idris --ide-mode"
    -- putStrLn (show 1234)

export
main : IO ()
main = do
    p <- initializeRepl
    -- pidBits <- prim_peek32 p 0
    -- putStrLn (show pidBits)
    -- let n = bitsToInt' {n=32} pidBits
    replMain SupportedCommands
