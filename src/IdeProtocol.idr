module IdeProtocol

import SExp

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
    
