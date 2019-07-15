module Main

import GenericRepl

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

export
main : IO ()
main = replMain SupportedCommands
