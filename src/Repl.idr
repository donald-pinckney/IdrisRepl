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
    currentFilePath : Maybe String

implementation Prompt ReplState where
    prompt = currentPrompt


total
handle_reply : ReplState -> Either String IdeReply -> IO ReplState
handle_reply s (Left err) = do putStrLn $ "Error: " ++ err; pure s
handle_reply s (Right (IdeReplyReturnOk x highlight)) = do
    (case x of
        (CSExpAtom (CAtomStr returnStr)) => putStrLn returnStr
        _ => pure ()
    )
    pure s
handle_reply s (Right (IdeReplyReturnErr x highlight)) = do
    putStrLn x
    pure s
handle_reply s (Right (IdeReplyOutputOk x xs)) = pure s
handle_reply s (Right (IdeReplyOutputErr x xs)) = pure s
handle_reply s (Right (IdeReplyWriteString str)) = do putStrLn str; pure s
handle_reply s (Right (IdeReplySetPrompt prmt)) = pure $ record { currentPrompt = prmt ++ "> "} s
handle_reply s (Right (IdeReplyWarning f l1 c1 l2 c2 str xs)) = do
    putStrLn (f ++ ":" ++ (show l1) ++ ":" ++ (show c1) ++ "--" ++ (show l2) ++ ":" ++ (show c2) ++ ":" ++ str)
    pure s


handling_command' : IdeCommand -> ReplCommand ReplState
handling_command' comm state = do
    Right () <- writeIdeCommand (write_f state) (reqId state) comm
        | Left err => pure (Left err)

    newState <- readUntilReturn (read_f state) (reqId state) state handle_reply

    pure $ Right $ Just ("", record { reqId $= (+ 1) } newState)

handling_command : IdeCommand -> Either String (ReplCommand ReplState)
handling_command comm = Right (handling_command' comm)


command_update_state : (ReplState -> ReplState) -> Either String (ReplCommand ReplState) -> Either String (ReplCommand ReplState)
command_update_state f (Left l) = Left l
command_update_state f (Right r) = Right $ \s => do
    Right (Just res) <- r s
        | Left err => pure (Left err)
        | Right Nothing => pure (Right Nothing)
    pure $ Right $ Just $ f <$> res


reload_command : ReplCommand ReplState
reload_command (MkReplState read_f write_f reqId currentPrompt Nothing) = pure $ Left "No file loaded"
reload_command state@(MkReplState read_f write_f reqId currentPrompt (Just cfp)) =
    handling_command' (IdeCommLoadFile cfp Nothing) state

SupportedCommands : List (CommandBuilder ReplState)
SupportedCommands = [
    MkCommandBuilder [['e','v','a','l']] "<expr>" "Evaluate an expression" (handling_command . IdeCommInterpret . pack),
    MkCommandBuilder [['l'],['l','o','a','d']] "<filename>" "Load a new file" (\path =>
        command_update_state (record { currentFilePath = Just (pack path) }) $
        handling_command (IdeCommLoadFile (pack path) Nothing)),
    MkCommandBuilder [['t'],['t','y','p','e']] "<expr>" "Check the type of an expression" (handling_command . IdeCommTypeOf . pack),
    MkCommandBuilder [['r'],(unpack "reload")] "" "Reload current file" (const (Right reload_command))
]

export
idrisRepl : File -> File -> IO ()
idrisRepl read_f write_f = replMain SupportedCommands (MkReplState read_f write_f 1 "Idris> " Nothing)
