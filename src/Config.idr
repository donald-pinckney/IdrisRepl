module Config

export
SHOULD_TRACE : Bool
SHOULD_TRACE = False

export
traceStrLn : String -> IO ()
traceStrLn str = if SHOULD_TRACE then putStrLn str else pure ()
