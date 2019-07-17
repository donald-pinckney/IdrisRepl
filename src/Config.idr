module Config

export
SHOULD_TRACE : Bool
SHOULD_TRACE = False

export
IDRIS_NAME : String
IDRIS_NAME = "idris2"

export
traceStrLn : String -> IO ()
traceStrLn str = if SHOULD_TRACE then putStrLn str else pure ()
