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
ReplCommand : Type -> Type
ReplCommand stateType = stateType -> IO (Result (Maybe (String, stateType)))

public export
record CommandBuilder stateType where
    constructor MkCommandBuilder
    commandStrs : List (List Char)
    argStrs : String
    docStrs : String
    buildFn : List Char -> Result (ReplCommand stateType)

helpRow : CommandBuilder stateType -> String
helpRow (MkCommandBuilder commandStrs argStrs docStrs buildFn) =
    let commandsStr = unwords $ map (\cmdStr => ":" ++ pack cmdStr) commandStrs in
    commandsStr ++ " " ++ argStrs ++ "\r\n\t" ++ docStrs


execReplCommand : stateType -> ReplCommand stateType -> IO (Result (Maybe (String, stateType)))
execReplCommand state f = f state

Command_nop : ReplCommand stateType
Command_nop = \s => pure $ Right $ Just ("", s)

Command_quit : ReplCommand stateType
Command_quit = \s => pure $ Right $ Nothing



parameters (supportedCommands : List (CommandBuilder stateType))
    mutual
        helpStr : String
        helpStr = unlines $ map helpRow actualSupportedCommands

        Command_help : ReplCommand stateType
        Command_help = \s => pure $ Right $ Just (helpStr, s)

        actualSupportedCommands : List (CommandBuilder stateType)
        actualSupportedCommands = supportedCommands ++ [
            MkCommandBuilder [['?'],['h'],['h','e','l','p']] "" "Display this help text." (\rest => pure Command_help),
            MkCommandBuilder [['q'],['q','u','i','t']] "" "Quits." (\rest => pure Command_quit)
        ]

    completions : List String
    completions =
        let commands' = map commandStrs actualSupportedCommands in
        let commands = join commands' in
        map (strCons ':' . pack) commands


    buildCommand : List Char -> List Char -> Result (ReplCommand stateType)
    buildCommand cmd rest =
        case find (\b => any (== cmd) (commandStrs b)) actualSupportedCommands of
            Nothing => Left $ "Unrecognized command: :" ++ (pack $ cmd) ++ ". Type :help to see a list of supported commands."
            (Just b) => (buildFn b) rest

    buildDefaultCommand : List Char -> Result (ReplCommand stateType)
    buildDefaultCommand rest =
        case head' supportedCommands of
            Nothing => Left $ "Error: No supported commands."
            (Just defaultCommand) => (buildFn defaultCommand) rest

    parseReplCommand : String -> Result (ReplCommand stateType)
    parseReplCommand w_cmdStr = do
        let cmdStr = eatWhitespace (unpack w_cmdStr)
        case cmdStr of
            [] => Right Command_nop
            (':' :: xs) => do
                let xs' = eatWhitespace xs
                (cmd, rest) <- firstWordSplit xs'
                buildCommand cmd rest
            (x :: xs) => buildDefaultCommand cmdStr


    replIteration : stateType -> String -> IO (Result (Maybe (String, stateType)))
    replIteration state inputStr =
        case parseReplCommand inputStr of
            (Left parseErr) => pure $ Left parseErr
            (Right command) => execReplCommand state command

    replIterationWrapped : stateType -> String -> IO (Maybe (String, stateType))
    replIterationWrapped state x = do
        res <- replIteration state x
        case res of
            (Left l) => pure $ Just ("Error: " ++ l ++ "\r\n", state)
            (Right Nothing) => pure $ Nothing
            (Right (Just (str, newState))) => pure $ Just (str, newState)

    export
    replMain : Prompt stateType => stateType -> IO ()
    replMain initialState = do
        baselineReplWith {completions=completions} initialState replIterationWrapped
