module Repl

import GenericRepl
import Shared

record ReplState where
    constructor MkReplState
    read_f : File
    write_f : File
    reqId : Int

Command_echo : String -> ReplCommand ReplState
Command_echo str state = do
    putStrLn "ECHO:"
    pure $ Right $ Just (str, state)

SupportedCommands : List (CommandBuilder ReplState)
SupportedCommands = [
    MkCommandBuilder [['e'],['e','c','h','o']]
        "<term>"
        "echos input."
        (pure . Command_echo . pack)
]

export
idrisRepl : File -> File -> IO ()
idrisRepl read_f write_f = replMain SupportedCommands (MkReplState read_f write_f 1)
