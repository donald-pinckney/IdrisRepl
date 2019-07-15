module GenericRepl

import BaselineRepl
import Shared

-- First, just some random util stuff

joinBy : List (List a) -> a -> List a
joinBy [] s = []
joinBy [xs] s = xs
joinBy (x :: xs) s = x ++ s :: (joinBy xs s)


firstWordSplit : List Char -> Result (List Char, List Char)
firstWordSplit xs =
    let words = split (==' ') xs in
    case words of
        [] => Left "Expected word"
        (w :: ws) => Right (w, joinBy ws ' ')


public export
ReplCommand : Type
ReplCommand = () -> IO (Result (Maybe String))

public export
record CommandBuilder where
    constructor MkCommandBuilder
    commandStrs : List (List Char)
    argStrs : String
    docStrs : String
    buildFn : List Char -> Result ReplCommand

helpRow : CommandBuilder -> String
helpRow (MkCommandBuilder commandStrs argStrs docStrs buildFn) =
    let commandsStr = unwords $ map (\cmdStr => ":" ++ pack cmdStr) commandStrs in
    commandsStr ++ " " ++ argStrs ++ "\r\n\t" ++ docStrs


execReplCommand : ReplCommand -> IO (Result (Maybe String))
execReplCommand f = f ()


Command_nop : ReplCommand
Command_nop = \_ => pure $ Right $ Just ""

Command_quit : ReplCommand
Command_quit = \_ => pure $ Right $ Nothing



parameters (supportedCommands : List CommandBuilder)
    mutual
        helpStr : String
        helpStr = unlines $ map helpRow actualSupportedCommands

        Command_help : ReplCommand
        Command_help = \_ => pure $ Right $ Just helpStr

        actualSupportedCommands : List CommandBuilder
        actualSupportedCommands = supportedCommands ++ [
            MkCommandBuilder [['?'],['h'],['h','e','l','p']] "" "Display this help text." (\rest => pure Command_help),
            MkCommandBuilder [['q'],['q','u','i','t']] "" "Quits." (\rest => pure Command_quit)
        ]

    completions : List String
    completions =
        let commands' = map commandStrs actualSupportedCommands in
        let commands = join commands' in
        map (strCons ':' . pack) commands


    buildCommand : List Char -> List Char -> Result ReplCommand
    buildCommand cmd rest =
        case find (\b => any (== cmd) (commandStrs b)) actualSupportedCommands of
            Nothing => Left $ "Unrecognized command: :" ++ (pack $ cmd) ++ ". Type :help to see a list of supported commands."
            (Just b) => (buildFn b) rest

    buildDefaultCommand : List Char -> Result ReplCommand
    buildDefaultCommand rest =
        case head' supportedCommands of
            Nothing => Left $ "Error: No supported commands."
            (Just defaultCommand) => (buildFn defaultCommand) rest

    parseReplCommand : String -> Result ReplCommand
    parseReplCommand w_cmdStr = do
        let cmdStr = eatWhitespace (unpack w_cmdStr)
        case cmdStr of
            [] => Right Command_nop
            (':' :: xs) => do
                let xs' = eatWhitespace xs
                (cmd, rest) <- firstWordSplit xs'
                buildCommand cmd rest
            (x :: xs) => buildDefaultCommand cmdStr


    replIteration : String -> IO (Result (Maybe String))
    replIteration inputStr =
        case parseReplCommand inputStr of
            (Left parseErr) => pure $ Left parseErr
            (Right command) => execReplCommand command

    replIterationWrapped : a -> String -> IO (Maybe (String, a))
    replIterationWrapped state x = do
        res <- replIteration x
        case res of
            (Left l) => pure $ Just ("Error: " ++ l ++ "\r\n", state)
            (Right Nothing) => pure $ Nothing
            (Right (Just a)) => pure $ Just (a ++ "\r\n", state)

    export
    replMain : IO ()
    replMain = baselineReplWith {completions=completions} () "Idris> " replIterationWrapped
