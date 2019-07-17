module Repl

import GenericRepl
import Shared
import IdeProtocol

record ReplState where
    constructor MkReplState
    read_f : File
    write_f : File
    reqId : Integer


Command_echo : String -> ReplCommand ReplState
Command_echo str state = do
    putStrLn "ECHO:"
    pure $ Right $ Just (str, state)

Command_load : String -> ReplCommand ReplState
Command_load path state = do
    Right () <- writeIdeCommand (write_f state) (reqId state) (IdeCommLoadFile path Nothing)
        | Left err => pure (Left err)

    Right line <- readIdeLine (read_f state)
    Right line <- readIdeLine (read_f state)
    Right line <- readIdeLine (read_f state)

    pure $ Right $ Just ("Load: " ++ path, state)


SupportedCommands : List (CommandBuilder ReplState)
SupportedCommands = [
    MkCommandBuilder [['e'],['e','c','h','o']]
        "<term>"
        "echos input."
        (pure . Command_echo . pack),
    MkCommandBuilder [['l'],['l','o','a','d']]
        "<filename>"
        "Load a new file"
        (pure . Command_load . pack)
]

export
idrisRepl : File -> File -> IO ()
idrisRepl read_f write_f = replMain SupportedCommands (MkReplState read_f write_f 1)
