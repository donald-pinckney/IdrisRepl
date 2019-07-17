module Repl

import GenericRepl
import Shared
import IdeProtocol
import BaselineRepl
import SExp

record ReplState where
    constructor MkReplState
    read_f : File
    write_f : File
    reqId : Integer
    currentPrompt : String

implementation Prompt ReplState where
    prompt = currentPrompt


total
handle_reply : ReplState -> Either String IdeReply -> IO ReplState
handle_reply s (Left err) = do putStrLn $ "Error: " ++ err; pure s
handle_reply s (Right (IdeReplyReturnOk x xs)) = pure s
handle_reply s (Right (IdeReplyReturnErr x xs)) = pure s
handle_reply s (Right (IdeReplyOutputOk x xs)) = pure s
handle_reply s (Right (IdeReplyOutputErr x xs)) = pure s
handle_reply s (Right (IdeReplyWriteString str)) = do putStrLn str; pure s
handle_reply s (Right (IdeReplySetPrompt prmt)) = pure $ record { currentPrompt = prmt ++ "> "} s
handle_reply s (Right (IdeReplyWarning f l1 c1 l2 c2 str xs)) = do
    putStrLn (f ++ ":" ++ (show l1) ++ ":" ++ (show c1) ++ "--" ++ (show l2) ++ ":" ++ (show c2) ++ ":" ++ str)
    pure s


Command_load : String -> ReplCommand ReplState
Command_load path state = do
    Right () <- writeIdeCommand (write_f state) (reqId state) (IdeCommLoadFile path Nothing)
        | Left err => pure (Left err)

    newState <- readUntilReturn (read_f state) (reqId state) state handle_reply

    pure $ Right $ Just ("", record { reqId $= (+ 1) } newState)


SupportedCommands : List (CommandBuilder ReplState)
SupportedCommands = [
    MkCommandBuilder [['l'],['l','o','a','d']]
        "<filename>"
        "Load a new file"
        (pure . Command_load . pack)
]

export
idrisRepl : File -> File -> IO ()
idrisRepl read_f write_f = replMain SupportedCommands (MkReplState read_f write_f 1 "Idris> ")
