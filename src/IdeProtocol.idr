module IdeProtocol

import SExp
import Pipes

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


export
readIdeLine : File -> IO (Either String String)
readIdeLine f = do
    Right lenStr <- fGetChars f 6
        | Left err => pure (Left (show err))
    let Just len = parseHex (unpack lenStr) 0
        | Nothing => pure (Left ("failed to parse hex: " ++ lenStr))

    Right payload <- fGetChars f len
        | Left err => pure (Left (show err))

    putStrLn $ "Received from pipe: " ++ payload

    pure (Right payload)

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

            putStrLn $ "Wrote to pipe: " ++ toWrite

            pure (Right ())

export
writeIdeCommand : File -> Integer -> IdeCommand -> IO (Either String ())
writeIdeCommand f id c =
    let comm = commandToSExp c in
    let request = CSExpList [comm, CSExpAtom $ CAtomNum id] in
    writeIdeLine (show request) f
