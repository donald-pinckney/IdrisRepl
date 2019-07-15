module SExp

import Shared
import Data.String

-- See: http://docs.idris-lang.org/en/latest/reference/ide-protocol.html

-- A ::= NUM | '"' STR '"' | ':' ALPHA+
-- S ::= A | '(' S* ')' | nil

public export
data CAtom = CAtomNum Integer | CAtomStr String | CAtomSymbol String

public export
data CSExp = CSExpAtom CAtom | CSExpList (List CSExp)

total
sexpStartChar : Char -> Bool
sexpStartChar '(' = True
sexpStartChar '"' = True
sexpStartChar ':' = True
sexpStartChar 'n' = True
sexpStartChar c = isDigit c


-- ... "*..."...     ---->   ... "..."*...
parse_str : List Char -> List Char -> Result (String, List Char)
parse_str ('\\' :: ('"' :: rest)) acc = parse_str rest ('"' :: acc)
parse_str ('\\' :: ('\\' :: rest)) acc = parse_str rest ('\\' :: acc)
parse_str ('\\' :: (x :: rest)) acc = Left $ "unrecognized escape char: " ++ singleton x
parse_str ('"' :: rest) acc = Right (reverse $ pack acc, rest)
parse_str (x :: rest) acc = parse_str rest (x :: acc)
parse_str [] acc = Left $ "unterminated string: " ++ (pack acc)

-- ... :*sym ...     ---->   ... :sym* ...
parse_symbol : List Char -> Result (String, List Char)
parse_symbol xs =
    let (symStr, rest) = span symbolChar xs in
    if length symStr == 0 then
        Left "symbol expected."
    else
        Right (pack symStr, rest)

    where
        symbolChar : Char -> Bool
        symbolChar '-' = True
        symbolChar x = isAlpha x

-- ... *234 ...     ---->   ... 234* ...
parse_num : List Char -> Result (Integer, List Char)
parse_num str =
    let (numStr, rest) = span isDigit str in
    case parseInteger (pack numStr) of
        Nothing => Left "number expected."
        (Just n) => Right (n, rest)



parse_atom : List Char -> Result (CAtom, List Char)
parse_atom [] = Left "atom expected."
parse_atom ('"' :: xs) = do
    (str, rest) <- parse_str xs []
    pure (CAtomStr str, rest)
parse_atom (':' :: xs) = do
    (str, rest) <- parse_symbol xs
    pure (CAtomSymbol str, rest)
parse_atom str@(x :: xs) = do
    (n, rest) <- parse_num str
    pure (CAtomNum n, rest)

mutual
    parse_sexp_list : List Char -> Result (List CSExp, List Char)
    parse_sexp_list [] = Right ([], [])
    parse_sexp_list str@(x :: xs) =
        if sexpStartChar x then do
            (h, rest) <- parse_sexp str
            let rest = eatWhitespace rest
            (tl, rest) <- parse_sexp_list rest
            Right (h :: tl, rest)
        else
            Right ([], str)

    parse_sexp : List Char -> Result (CSExp, List Char)
    parse_sexp [] = Left "sexp expected."
    parse_sexp ('n' :: 'i' :: 'l' :: rest) = Right (CSExpList [], rest)
    parse_sexp ('(' :: rest) = do
        (list, rest) <- parse_sexp_list rest
        rest <- expect rest ')'
        Right (CSExpList list, rest)
    parse_sexp str = do
        (atom, rest) <- parse_atom str
        pure (CSExpAtom atom, rest)

export
parse_sexp_main : List Char -> Result CSExp
parse_sexp_main xs = do
    (sexp, rest) <- parse_sexp xs
    if length rest == 0 then
        pure sexp
    else
        Left "incomplete parse."

export
implementation Show CAtom where
    show (CAtomNum n) = show n
    show (CAtomStr str) = show str
    show (CAtomSymbol sym) = ':' `strCons` sym

export
implementation Show CSExp where
    show (CSExpAtom x) = show x
    show (CSExpList xs) = "(" ++ (unwords (map show xs)) ++ ")"
