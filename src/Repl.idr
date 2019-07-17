module Repl

import GenericRepl
import Shared
import IdeProtocol

record ReplState where
    constructor MkReplState
    read_f : File
    write_f : File
    reqId : Integer


Command_load : String -> ReplCommand ReplState
Command_load path state = do
    Right () <- writeIdeCommand (write_f state) (reqId state) (IdeCommLoadFile path Nothing)
        | Left err => pure (Left err)

    Right reply <- readIdeReply_trace (read_f state) (reqId state)
    Right reply <- readIdeReply_trace (read_f state) (reqId state)
    Right reply <- readIdeReply_trace (read_f state) (reqId state)

    pure $ Right $ Just ("Load: " ++ path, record { reqId $= (+ 1) } state)


SupportedCommands : List (CommandBuilder ReplState)
SupportedCommands = [
    MkCommandBuilder [['l'],['l','o','a','d']]
        "<filename>"
        "Load a new file"
        (pure . Command_load . pack)
]

export
idrisRepl : File -> File -> IO ()
idrisRepl read_f write_f = replMain SupportedCommands (MkReplState read_f write_f 1)
