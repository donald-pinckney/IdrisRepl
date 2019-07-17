module IdeProtocol

import SExp
import Pipes
import Config

-- See: http://docs.idris-lang.org/en/latest/reference/ide-protocol.html

public export
data EditCommand
    = EditCommCaseSplit Integer String
    | EditCommAddClause Integer String
    | EditCommAddProofClause Integer String
    | EditCommAddMissing Integer String
    | EditCommMakeWith Integer String
    | EditCommMakeCase Integer String
    | EditCommMakeLemma Integer String
    | EditCommProofSearch Integer String (List String)

public export
data DocMode = Full | Overview

public export
TTTerm : Type
TTTerm = String

public export
data IdeCommand
    = IdeCommLoadFile String (Maybe Integer)
    | IdeCommInterpret String
    | IdeCommTypeOf String
    | IdeCommEdit EditCommand
    | IdeCommDocsFor String DocMode
    | IdeCommApropos String
    | IdeCommMetavariables Integer
    | IdeCommWhoCalls String
    | IdeCommCallsWho String
    | IdeCommBrowseNamespace String
    | IdeCommNormalizeTerm TTTerm
    | IdeCommShowTermImplicits TTTerm
    | IdeCommHideTermImplicits TTTerm
    | IdeCommElaborateTerm TTTerm
    | IdeCommPrintDefinition String
    | IdeCommReplCompletions String
    | IdeCommVersion


atoms : List CAtom -> CSExp
atoms xs = CSExpList (map CSExpAtom xs)

commandToSExp : IdeCommand -> CSExp
commandToSExp (IdeCommLoadFile x Nothing) = atoms [CAtomSymbol "load-file", CAtomStr x]
commandToSExp (IdeCommLoadFile x (Just y)) = atoms [CAtomSymbol "load-file", CAtomStr x, CAtomNum y]
commandToSExp (IdeCommInterpret x) = ?commandToSExp_rhs_2
commandToSExp (IdeCommTypeOf x) = ?commandToSExp_rhs_3
commandToSExp (IdeCommEdit x) = ?commandToSExp_rhs_4
commandToSExp (IdeCommDocsFor x Full) = ?commandToSExp_rhs_1
commandToSExp (IdeCommDocsFor x Overview) = ?commandToSExp_rhs_18
commandToSExp (IdeCommApropos x) = ?commandToSExp_rhs_6
commandToSExp (IdeCommMetavariables x) = ?commandToSExp_rhs_7
commandToSExp (IdeCommWhoCalls x) = ?commandToSExp_rhs_8
commandToSExp (IdeCommCallsWho x) = ?commandToSExp_rhs_9
commandToSExp (IdeCommBrowseNamespace x) = ?commandToSExp_rhs_10
commandToSExp (IdeCommNormalizeTerm x) = ?commandToSExp_rhs_11
commandToSExp (IdeCommShowTermImplicits x) = ?commandToSExp_rhs_12
commandToSExp (IdeCommHideTermImplicits x) = ?commandToSExp_rhs_13
commandToSExp (IdeCommElaborateTerm x) = ?commandToSExp_rhs_14
commandToSExp (IdeCommPrintDefinition x) = ?commandToSExp_rhs_15
commandToSExp (IdeCommReplCompletions x) = ?commandToSExp_rhs_16
commandToSExp IdeCommVersion = ?commandToSExp_rhs_17

-- TODO
public export
data SemanticData

public export
SemanticSpan : Type
SemanticSpan = (Integer, Integer, List SemanticData)

public export
data IdeReply
    = IdeReplyReturnOk CSExp (List SemanticSpan)
    | IdeReplyReturnErr String (List SemanticSpan)
    | IdeReplyOutputOk CSExp (List SemanticSpan)
    | IdeReplyOutputErr String (List SemanticSpan)
    | IdeReplyWriteString String
    | IdeReplySetPrompt String
    | IdeReplyWarning String Integer Integer Integer Integer String (List SemanticSpan)
    -- (:warning (FilePath (LINE COL) (LINE COL) String [HIGHLIGHTING]))

export
implementation Show IdeReply where
    show (IdeReplyReturnOk x xs) = "return, ok: "
    show (IdeReplyReturnErr x xs) = "return, error: " ++ (show x)
    show (IdeReplyOutputOk x xs) = "output, ok: " ++ (show x)
    show (IdeReplyOutputErr x xs) = "output, error: " ++ (show x)
    show (IdeReplyWriteString x) = "write string: " ++ (show x)
    show (IdeReplySetPrompt x) = "set prompt: " ++ (show x)
    show (IdeReplyWarning path l1 c1 l2 c2 str xs) = "warning: " ++ path ++ "\n\t" ++ str

export
readIdeLine : File -> IO (Either String String)
readIdeLine f =
    do
        Right lenStr <- fGetChars f 6
            | Left err => pure (Left (show err))
        let Just len = parseHex (unpack lenStr) 0
            | Nothing => pure (Left ("failed to parse hex: " ++ lenStr))

        Right payloadNL <- fGetChars f len
            | Left err => pure (Left (show err))

        let payload = reverse (trimNL (reverse payloadNL))
        traceStrLn $ "[PIPE] Received from pipe: " ++ payload
        pure (Right payload)

    where
        trimNL : String -> String
        trimNL str with (strM str)
            trimNL "" | StrNil = ""
            trimNL (strCons '\n' xs) | StrCons _ _ = xs
            trimNL (strCons x xs)    | StrCons _ _ = strCons x xs

export
writeIdeLine : String -> File -> IO (Either String ())
writeIdeLine str f =
    let lineLen = 1 + (fromNat (length str)) in
    case printHex6 lineLen of
    Nothing => pure $ Left "Line too long!"
    (Just lenStr) =>
        let toWrite = lenStr ++ str in
        do
            Right () <- fPutStrLn f toWrite
                | Left err => pure (Left (show err))

            traceStrLn $ "[PIPE] Wrote to pipe: " ++ toWrite

            pure (Right ())

export
writeIdeCommand : File -> Integer -> IdeCommand -> IO (Either String ())
writeIdeCommand f id c =
    let comm = commandToSExp c in
    let request = CSExpList [comm, CSExpAtom $ CAtomNum id] in
    writeIdeLine (show request) f


readIdeReply' : File -> Integer -> IO (Either String IdeReply)
readIdeReply' f id = do
    Right line <- readIdeLine f
        | Left err => pure (Left err)

    let Right (CSExpList replyArgs') = parse_sexp_main (unpack line)
        | Left err => pure (Left $ "parse error: " ++ err)
        | Right (CSExpAtom a) => pure (Left $ "expected list, got atom: " ++ (show a))

    -- Validate request ID
    let (CSExpAtom (CAtomNum replyId) :: replyArgs_rev') = reverse replyArgs'
        | [] => pure (Left $ "expected non-empty list")
        | (h :: rest) => pure (Left $ "expected to receive request id as last element, instead got: " ++ (show replyArgs'))
    let True = id == replyId
        | False => pure (Left $ "expected request id " ++ (show id) ++ ", instead received id " ++ (show replyId))
    let replyArgs = reverse replyArgs_rev'

    -- Parse reply into enum
    pure $ case replyArgs of
        [] => (Left $ "expected list of length greater than 1, got: " ++ (show replyArgs'))
        [CSExpAtom $ CAtomSymbol "return", CSExpList (
            (CSExpAtom $ CAtomSymbol "ok") ::
            sexp ::
            hightlight)] =>
            Right $ IdeReplyReturnOk sexp []
        [CSExpAtom $ CAtomSymbol "return", CSExpList (
            (CSExpAtom $ CAtomSymbol "error") ::
            (CSExpAtom $ CAtomStr error) ::
            hightlight)] =>
            Right $ IdeReplyReturnErr error []
        [CSExpAtom $ CAtomSymbol "output", CSExpList (
            (CSExpAtom $ CAtomSymbol "ok") ::
            sexp ::
            hightlight)] =>
            Right $ IdeReplyOutputOk sexp []
        [CSExpAtom $ CAtomSymbol "output", CSExpList (
            (CSExpAtom $ CAtomSymbol "error") ::
            (CSExpAtom $ CAtomStr error) ::
            hightlight)] =>
            Right $ IdeReplyOutputErr error []
        [CSExpAtom $ CAtomSymbol "write-string", CSExpAtom $ CAtomStr str] =>
            Right $ IdeReplyWriteString str
        [CSExpAtom $ CAtomSymbol "set-prompt", CSExpAtom $ CAtomStr str] =>
            Right $ IdeReplySetPrompt str
        [CSExpAtom $ CAtomSymbol "warning", CSExpList (
            (CSExpAtom $ CAtomStr filePath) ::
            (CSExpList [CSExpAtom $ CAtomNum line1, CSExpAtom $ CAtomNum col1]) ::
            (CSExpList [CSExpAtom $ CAtomNum line2, CSExpAtom $ CAtomNum col2]) ::
            (CSExpAtom $ CAtomStr str) ::
            hightlight)] =>
            Right $ IdeReplyWarning filePath line1 col1 line2 col2 str []
        _ => (Left $ "Do not understand: " ++ (show replyArgs))

export
readIdeReply : File -> Integer -> IO (Either String IdeReply)
readIdeReply f id = do
    Right reply <- readIdeReply' f id
        | Left err => pure (Left err)

    traceStrLn ("[REPLY] Received reply: " ++ (show reply))
    pure (Right reply)

export
readUntilReturn : File -> Integer -> a -> (a -> Either String IdeReply -> IO a) -> IO a
readUntilReturn f id state callback = do
    Right reply <- readIdeReply f id
        | Left err => (callback state (Left err))

    newState <- callback state (Right reply)

    case reply of
        (IdeReplyReturnOk x xs) => pure newState
        (IdeReplyReturnErr x xs) => pure newState
        _ => do
            readUntilReturn f id newState callback
