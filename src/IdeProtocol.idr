module IdeProtocol

import SExp
import Pipes

-- See: http://docs.idris-lang.org/en/latest/reference/ide-protocol.html

data EditCommand
    = EditCommCaseSplit Integer String
    | EditCommAddClause Integer String
    | EditCommAddProofClause Integer String
    | EditCommAddMissing Integer String
    | EditCommMakeWith Integer String
    | EditCommMakeCase Integer String
    | EditCommMakeLemma Integer String
    | EditCommProofSearch Integer String (List String)

data DocMode = Full | Overview

TTTerm : Type
TTTerm = String

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
