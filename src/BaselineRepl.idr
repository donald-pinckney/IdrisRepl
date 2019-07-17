module BaselineRepl

import Baseline

export
interface Prompt a where
    prompt : a -> String


baselineReplWith_loop : Prompt a => a -> (a -> String -> IO (Maybe (String, a))) -> IO ()
baselineReplWith_loop state f = do
    str <- baseline (prompt state)
    case str of
        Nothing => pure ()
        Just line => do
            res <- f state line
            case res of
                Nothing => pure ()
                Just (out, state') => do
                    putStr out
                    baselineReplWith_loop state' f

export
baselineReplWith : Prompt a => {default [] completions : List String} -> a -> (a -> String -> IO (Maybe (String, a))) -> IO ()
baselineReplWith {completions} state f = do
    readHistory ".history"
    addDictEntries completions
    baselineReplWith_loop state f
    writeHistory ".history"
