module Shared

import Config

public export
Result : Type -> Type
Result a = Either String a

export
eatWhitespace : List Char -> List Char
eatWhitespace [] = []
eatWhitespace (x :: xs) =
    if x == ' ' || x == '\n' || x == '\t' || x == '\r' then
        eatWhitespace xs
    else
        (x :: xs)

export
expect : List Char -> Char -> Result (List Char)
expect [] c = Left $ "Expected '" ++ (singleton c) ++ "', but no input left."
expect (cx :: xs) c =
    if cx == c then
        Right xs
    else
        Left $ "Expected '" ++ (singleton c) ++ "', got '" ++ (singleton cx) ++ "'"

export
fatalError : Show a => a -> b
fatalError x =
    idris_crash (show x)

export
traceStrLn : String -> IO ()
traceStrLn str = if SHOULD_TRACE then putStrLn str else pure ()
