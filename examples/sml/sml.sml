functor Sml (
  val table_size : int
  structure Stream : STREAM
  structure Trivial : TERMINAL where type 'a stream = 'a Stream.stream
  structure Terminals : sig
    structure Char : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
    structure Float : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
    structure Id : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
    structure Int : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
    structure String : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
    structure Tyvar : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
    structure Word : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
  end
) :>

sig
  type 'a annot = { node : 'a , span : Annot.span }
  datatype con' = ConInt of Terminals.Int.t annot
  | ConWord of Terminals.Word.t annot
  | ConFloat of Terminals.Float.t annot
  | ConChar of Terminals.Char.t annot
  | ConString of Terminals.String.t annot
  and lab' = LabId of Terminals.Id.t annot
  | LabNum of Terminals.Int.t annot
  and long_id' = LongIdLongId of Terminals.Id.t annot * Terminals.Id.t annot list
  and exp' = ExpConst of con
  | ExpOpId of long_id
  | ExpId of long_id
  | ExpApp of exp * exp
  | ExpParens of exp
  | ExpTuple of exp * exp * exp list
  | ExpRecord of exp_row option
  | ExpSelector of lab
  | ExpList of exp_list_inner option
  | ExpSeq of exp * exp list
  | ExpLet of dec_list * exp * exp list
  | ExpAnnot of exp * typ
  | ExpRaise of exp
  | ExpHandle of exp * match
  | ExpAndAlso of exp * exp
  | ExpOrElse of exp * exp
  | ExpIf of exp * exp * exp
  | ExpWhile of exp * exp
  | ExpCase of exp * match
  | ExpFn of match
  and exp_list_inner' = ExpListInnerExpListInner of exp * exp list
  and exp_row' = ExpRowExpRow of lab * exp * (lab * exp) list
  and match' = MatchMatch of match_arm * match_arm list
  and match_arm' = MatchArmMatchArm of pat * exp
  and pat' = PatConst of con
  | PatWildcard
  | PatOpVar of Terminals.Id.t annot
  | PatVar of Terminals.Id.t annot
  | PatOpCon of long_id * pat option
  | PatCon of long_id * pat
  | PatParens of pat
  | PatTuple of pat * pat * pat list
  | PatRecord of pat_row option
  | PatList of pat_list_inner option
  | PatAnnot of pat * typ
  | PatOpLayered of Terminals.Id.t annot * typ option * pat
  | PatLayered of Terminals.Id.t annot * typ option * pat
  and pat_list_inner' = PatListInnerPatListInner of pat * pat list
  and pat_row' = PatRowWildcard
  | PatRowPat of lab * pat * pat_row option
  | PatRowVar of Terminals.Id.t annot * typ option * pat option * pat_row option
  and typ' = TypVar of Terminals.Tyvar.t annot
  | TypConApp of typ * long_id
  | TypConAppMulti of typ * typ * typ list * long_id
  | TypCon of long_id
  | TypParens of typ
  | TypArrow of typ * typ
  | TypTupleTyp of typ * typ list
  | TypRecord of typ_row option
  and typ_row' = TypRowTypRow of lab * typ * (lab * typ) list
  and dec' = DecVal of ty_var_seq * val_bind
  | DecFun of ty_var_seq * fun_bind
  | DecType of typ_bind
  | DecDatatype of dat_bind * typ_bind option
  | DecDatatypeRepl of Terminals.Id.t annot * long_id
  | DecAbstype of dat_bind * typ_bind option * dec_list
  | DecException of exn_bind
  | DecStructure of str_bind
  | DecSemicolon
  | DecLocal of dec_list * dec_list
  | DecOpen of long_id * long_id list
  | DecNonfix of Terminals.Id.t annot * Terminals.Id.t annot list
  | DecInfix of Terminals.Int.t annot option * Terminals.Id.t annot * Terminals.Id.t annot list
  | DecInfixr of Terminals.Int.t annot option * Terminals.Id.t annot * Terminals.Id.t annot list
  and dec_list' = DecListDecList of dec list
  and ty_var_seq' = TyVarSeqOne of Terminals.Tyvar.t annot
  | TyVarSeqMany of Terminals.Tyvar.t annot * Terminals.Tyvar.t annot list
  | TyVarSeqEmpty
  and val_bind' = ValBindValBind of pat * exp * val_bind option
  | ValBindRec of val_bind
  and fun_bind' = FunBindFunBind of fun_match * fun_bind option
  and fun_match' = FunMatchNonfix of Terminals.Id.t annot * pat * pat list * typ option * exp * fun_match option
  | FunMatchInfix of pat * Terminals.Id.t annot * pat * typ option * exp * fun_match option
  | FunMatchInfixParen of pat * Terminals.Id.t annot * pat * pat list * typ option * exp * fun_match option
  and typ_bind' = TypBindTypBind of ty_var_seq * Terminals.Id.t annot * typ * typ_bind option
  and dat_bind' = DatBindDatBind of ty_var_seq * Terminals.Id.t annot * con_bind * dat_bind option
  and con_bind' = ConBindConBind of Terminals.Id.t annot * typ option * con_bind option
  and exn_bind' = ExnBindGen of Terminals.Id.t annot * typ option * exn_bind option
  | ExnBindRepl of Terminals.Id.t annot * long_id * exn_bind option
  and str' = StrId of long_id
  | StrStruct of dec_list
  | StrTransparent of str * sig_exp
  | StrOpaque of str * sig_exp
  | StrFctApp of Terminals.Id.t annot * str
  | StrFctAppDec of Terminals.Id.t annot * dec_list
  | StrLet of dec_list * str
  and str_bind' = StrBindStrBind of Terminals.Id.t annot * sig_annot option * str * str_bind option
  and sig_annot' = SigAnnotTransparent of sig_exp
  | SigAnnotOpaque of sig_exp
  and sig_exp' = SigExpId of Terminals.Id.t annot
  | SigExpSig of spec_list
  | SigExpWhere of sig_exp * typ_refin
  and typ_refin' = TypRefinTypRefin of ty_var_seq * long_id * typ * typ_refin option
  and spec' = SpecVal of val_desc
  | SpecType of typ_desc
  | SpecEqtype of typ_desc
  | SpecTypeAbbrev of typ_bind
  | SpecDatatype of dat_desc
  | SpecDatatypeRepl of Terminals.Id.t annot * long_id
  | SpecException of exn_desc
  | SpecStructure of str_desc
  | SpecSemicolon
  | SpecInclude of sig_exp
  | SpecIncludeMulti of Terminals.Id.t annot * Terminals.Id.t annot list
  | SpecSharingType of spec * long_id * long_id list
  | SpecSharing of spec * long_id * long_id list
  and spec_list' = SpecListSpecList of spec list
  and val_desc' = ValDescValDesc of Terminals.Id.t annot * typ * val_desc option
  and typ_desc' = TypDescTypDesc of ty_var_seq * Terminals.Id.t annot * typ_desc option
  and dat_desc' = DatDescDatDesc of ty_var_seq * Terminals.Id.t annot * con_desc * dat_desc option
  and con_desc' = ConDescConDesc of Terminals.Id.t annot * typ option * con_desc option
  and exn_desc' = ExnDescExnDesc of Terminals.Id.t annot * typ option * exn_desc option
  and str_desc' = StrDescStrDesc of Terminals.Id.t annot * sig_exp * str_desc option
  and prog' = ProgDec of dec
  | ProgFunctor of fct_bind
  | ProgSignature of sig_bind
  | ProgSemicolon
  and prog_list' = ProgListProgList of prog list
  and fct_bind' = FctBindPlain of Terminals.Id.t annot * Terminals.Id.t annot * sig_exp * sig_annot option * str * fct_bind option
  | FctBindOpened of Terminals.Id.t annot * spec * sig_annot option * str * fct_bind option
  and sig_bind' = SigBindSigBind of Terminals.Id.t annot * sig_exp * sig_bind option
  withtype con = con' annot
  and lab = lab' annot
  and long_id = long_id' annot
  and exp = exp' annot
  and exp_list_inner = exp_list_inner' annot
  and exp_row = exp_row' annot
  and match = match' annot
  and match_arm = match_arm' annot
  and pat = pat' annot
  and pat_list_inner = pat_list_inner' annot
  and pat_row = pat_row' annot
  and typ = typ' annot
  and typ_row = typ_row' annot
  and dec = dec' annot
  and dec_list = dec_list' annot
  and ty_var_seq = ty_var_seq' annot
  and val_bind = val_bind' annot
  and fun_bind = fun_bind' annot
  and fun_match = fun_match' annot
  and typ_bind = typ_bind' annot
  and dat_bind = dat_bind' annot
  and con_bind = con_bind' annot
  and exn_bind = exn_bind' annot
  and str = str' annot
  and str_bind = str_bind' annot
  and sig_annot = sig_annot' annot
  and sig_exp = sig_exp' annot
  and typ_refin = typ_refin' annot
  and spec = spec' annot
  and spec_list = spec_list' annot
  and val_desc = val_desc' annot
  and typ_desc = typ_desc' annot
  and dat_desc = dat_desc' annot
  and con_desc = con_desc' annot
  and exn_desc = exn_desc' annot
  and str_desc = str_desc' annot
  and prog = prog' annot
  and prog_list = prog_list' annot
  and fct_bind = fct_bind' annot
  and sig_bind = sig_bind' annot
  
  type 'a parser
  type token_stream
  val lex : char Stream.stream -> Annot.pos -> token_stream
  val parseCon : con parser
  val parseLab : lab parser
  val parseLongId : long_id parser
  val parseExp : exp parser
  val parseExpListInner : exp_list_inner parser
  val parseExpRow : exp_row parser
  val parseMatch : match parser
  val parseMatchArm : match_arm parser
  val parsePat : pat parser
  val parsePatListInner : pat_list_inner parser
  val parsePatRow : pat_row parser
  val parseTyp : typ parser
  val parseTypRow : typ_row parser
  val parseDec : dec parser
  val parseDecList : dec_list parser
  val parseTyVarSeq : ty_var_seq parser
  val parseValBind : val_bind parser
  val parseFunBind : fun_bind parser
  val parseFunMatch : fun_match parser
  val parseTypBind : typ_bind parser
  val parseDatBind : dat_bind parser
  val parseConBind : con_bind parser
  val parseExnBind : exn_bind parser
  val parseStr : str parser
  val parseStrBind : str_bind parser
  val parseSigAnnot : sig_annot parser
  val parseSigExp : sig_exp parser
  val parseTypRefin : typ_refin parser
  val parseSpec : spec parser
  val parseSpecList : spec_list parser
  val parseValDesc : val_desc parser
  val parseTypDesc : typ_desc parser
  val parseDatDesc : dat_desc parser
  val parseConDesc : con_desc parser
  val parseExnDesc : exn_desc parser
  val parseStrDesc : str_desc parser
  val parseProg : prog parser
  val parseProgList : prog_list parser
  val parseFctBind : fct_bind parser
  val parseSigBind : sig_bind parser
  val parse : 'a parser -> token_stream -> ('a * token_stream) list
end =
struct

  type 'a annot = { node : 'a , span : Annot.span }
  
  datatype con' = ConInt of Terminals.Int.t annot
    | ConWord of Terminals.Word.t annot
    | ConFloat of Terminals.Float.t annot
    | ConChar of Terminals.Char.t annot
    | ConString of Terminals.String.t annot
  and lab' = LabId of Terminals.Id.t annot
    | LabNum of Terminals.Int.t annot
  and long_id' = LongIdLongId of Terminals.Id.t annot * Terminals.Id.t annot list
  and exp' = ExpConst of con
    | ExpOpId of long_id
    | ExpId of long_id
    | ExpApp of exp * exp
    | ExpParens of exp
    | ExpTuple of exp * exp * exp list
    | ExpRecord of exp_row option
    | ExpSelector of lab
    | ExpList of exp_list_inner option
    | ExpSeq of exp * exp list
    | ExpLet of dec_list * exp * exp list
    | ExpAnnot of exp * typ
    | ExpRaise of exp
    | ExpHandle of exp * match
    | ExpAndAlso of exp * exp
    | ExpOrElse of exp * exp
    | ExpIf of exp * exp * exp
    | ExpWhile of exp * exp
    | ExpCase of exp * match
    | ExpFn of match
  and exp_list_inner' = ExpListInnerExpListInner of exp * exp list
  and exp_row' = ExpRowExpRow of lab * exp * (lab * exp) list
  and match' = MatchMatch of match_arm * match_arm list
  and match_arm' = MatchArmMatchArm of pat * exp
  and pat' = PatConst of con
    | PatWildcard
    | PatOpVar of Terminals.Id.t annot
    | PatVar of Terminals.Id.t annot
    | PatOpCon of long_id * pat option
    | PatCon of long_id * pat
    | PatParens of pat
    | PatTuple of pat * pat * pat list
    | PatRecord of pat_row option
    | PatList of pat_list_inner option
    | PatAnnot of pat * typ
    | PatOpLayered of Terminals.Id.t annot * typ option * pat
    | PatLayered of Terminals.Id.t annot * typ option * pat
  and pat_list_inner' = PatListInnerPatListInner of pat * pat list
  and pat_row' = PatRowWildcard
    | PatRowPat of lab * pat * pat_row option
    | PatRowVar of Terminals.Id.t annot * typ option * pat option * pat_row option
  and typ' = TypVar of Terminals.Tyvar.t annot
    | TypConApp of typ * long_id
    | TypConAppMulti of typ * typ * typ list * long_id
    | TypCon of long_id
    | TypParens of typ
    | TypArrow of typ * typ
    | TypTupleTyp of typ * typ list
    | TypRecord of typ_row option
  and typ_row' = TypRowTypRow of lab * typ * (lab * typ) list
  and dec' = DecVal of ty_var_seq * val_bind
    | DecFun of ty_var_seq * fun_bind
    | DecType of typ_bind
    | DecDatatype of dat_bind * typ_bind option
    | DecDatatypeRepl of Terminals.Id.t annot * long_id
    | DecAbstype of dat_bind * typ_bind option * dec_list
    | DecException of exn_bind
    | DecStructure of str_bind
    | DecSemicolon
    | DecLocal of dec_list * dec_list
    | DecOpen of long_id * long_id list
    | DecNonfix of Terminals.Id.t annot * Terminals.Id.t annot list
    | DecInfix of Terminals.Int.t annot option * Terminals.Id.t annot * Terminals.Id.t annot list
    | DecInfixr of Terminals.Int.t annot option * Terminals.Id.t annot * Terminals.Id.t annot list
  and dec_list' = DecListDecList of dec list
  and ty_var_seq' = TyVarSeqOne of Terminals.Tyvar.t annot
    | TyVarSeqMany of Terminals.Tyvar.t annot * Terminals.Tyvar.t annot list
    | TyVarSeqEmpty
  and val_bind' = ValBindValBind of pat * exp * val_bind option
    | ValBindRec of val_bind
  and fun_bind' = FunBindFunBind of fun_match * fun_bind option
  and fun_match' = FunMatchNonfix of Terminals.Id.t annot * pat * pat list * typ option * exp * fun_match option
    | FunMatchInfix of pat * Terminals.Id.t annot * pat * typ option * exp * fun_match option
    | FunMatchInfixParen of pat * Terminals.Id.t annot * pat * pat list * typ option * exp * fun_match option
  and typ_bind' = TypBindTypBind of ty_var_seq * Terminals.Id.t annot * typ * typ_bind option
  and dat_bind' = DatBindDatBind of ty_var_seq * Terminals.Id.t annot * con_bind * dat_bind option
  and con_bind' = ConBindConBind of Terminals.Id.t annot * typ option * con_bind option
  and exn_bind' = ExnBindGen of Terminals.Id.t annot * typ option * exn_bind option
    | ExnBindRepl of Terminals.Id.t annot * long_id * exn_bind option
  and str' = StrId of long_id
    | StrStruct of dec_list
    | StrTransparent of str * sig_exp
    | StrOpaque of str * sig_exp
    | StrFctApp of Terminals.Id.t annot * str
    | StrFctAppDec of Terminals.Id.t annot * dec_list
    | StrLet of dec_list * str
  and str_bind' = StrBindStrBind of Terminals.Id.t annot * sig_annot option * str * str_bind option
  and sig_annot' = SigAnnotTransparent of sig_exp
    | SigAnnotOpaque of sig_exp
  and sig_exp' = SigExpId of Terminals.Id.t annot
    | SigExpSig of spec_list
    | SigExpWhere of sig_exp * typ_refin
  and typ_refin' = TypRefinTypRefin of ty_var_seq * long_id * typ * typ_refin option
  and spec' = SpecVal of val_desc
    | SpecType of typ_desc
    | SpecEqtype of typ_desc
    | SpecTypeAbbrev of typ_bind
    | SpecDatatype of dat_desc
    | SpecDatatypeRepl of Terminals.Id.t annot * long_id
    | SpecException of exn_desc
    | SpecStructure of str_desc
    | SpecSemicolon
    | SpecInclude of sig_exp
    | SpecIncludeMulti of Terminals.Id.t annot * Terminals.Id.t annot list
    | SpecSharingType of spec * long_id * long_id list
    | SpecSharing of spec * long_id * long_id list
  and spec_list' = SpecListSpecList of spec list
  and val_desc' = ValDescValDesc of Terminals.Id.t annot * typ * val_desc option
  and typ_desc' = TypDescTypDesc of ty_var_seq * Terminals.Id.t annot * typ_desc option
  and dat_desc' = DatDescDatDesc of ty_var_seq * Terminals.Id.t annot * con_desc * dat_desc option
  and con_desc' = ConDescConDesc of Terminals.Id.t annot * typ option * con_desc option
  and exn_desc' = ExnDescExnDesc of Terminals.Id.t annot * typ option * exn_desc option
  and str_desc' = StrDescStrDesc of Terminals.Id.t annot * sig_exp * str_desc option
  and prog' = ProgDec of dec
    | ProgFunctor of fct_bind
    | ProgSignature of sig_bind
    | ProgSemicolon
  and prog_list' = ProgListProgList of prog list
  and fct_bind' = FctBindPlain of Terminals.Id.t annot * Terminals.Id.t annot * sig_exp * sig_annot option * str * fct_bind option
    | FctBindOpened of Terminals.Id.t annot * spec * sig_annot option * str * fct_bind option
  and sig_bind' = SigBindSigBind of Terminals.Id.t annot * sig_exp * sig_bind option
  withtype con = con' annot
  and lab = lab' annot
  and long_id = long_id' annot
  and exp = exp' annot
  and exp_list_inner = exp_list_inner' annot
  and exp_row = exp_row' annot
  and match = match' annot
  and match_arm = match_arm' annot
  and pat = pat' annot
  and pat_list_inner = pat_list_inner' annot
  and pat_row = pat_row' annot
  and typ = typ' annot
  and typ_row = typ_row' annot
  and dec = dec' annot
  and dec_list = dec_list' annot
  and ty_var_seq = ty_var_seq' annot
  and val_bind = val_bind' annot
  and fun_bind = fun_bind' annot
  and fun_match = fun_match' annot
  and typ_bind = typ_bind' annot
  and dat_bind = dat_bind' annot
  and con_bind = con_bind' annot
  and exn_bind = exn_bind' annot
  and str = str' annot
  and str_bind = str_bind' annot
  and sig_annot = sig_annot' annot
  and sig_exp = sig_exp' annot
  and typ_refin = typ_refin' annot
  and spec = spec' annot
  and spec_list = spec_list' annot
  and val_desc = val_desc' annot
  and typ_desc = typ_desc' annot
  and dat_desc = dat_desc' annot
  and con_desc = con_desc' annot
  and exn_desc = exn_desc' annot
  and str_desc = str_desc' annot
  and prog = prog' annot
  and prog_list = prog_list' annot
  and fct_bind = fct_bind' annot
  and sig_bind = sig_bind' annot
  
  datatype terminal_token = TerminalChar of Terminals.Char.t
  | TerminalFloat of Terminals.Float.t
  | TerminalId of Terminals.Id.t
  | TerminalInt of Terminals.Int.t
  | TerminalString of Terminals.String.t
  | TerminalTyvar of Terminals.Tyvar.t
  | TerminalWord of Terminals.Word.t
  
  structure Internal = ParseInternal (
    val table_size = table_size
    structure Stream = Stream
    structure Trivial = Trivial
    type terminal = terminal_token
    val keywords =
      [ ("#" , 0)
      , ("(" , 1)
      , (")" , 2)
      , ("*" , 3)
      , ("," , 4)
      , ("->" , 5)
      , ("." , 6)
      , ("..." , 7)
      , (":" , 8)
      , (":>" , 9)
      , (";" , 10)
      , ("=" , 11)
      , ("=>" , 12)
      , ("[" , 13)
      , ("]" , 14)
      , ("_" , 15)
      , ("abstype" , 16)
      , ("and" , 17)
      , ("andalso" , 18)
      , ("as" , 19)
      , ("case" , 20)
      , ("datatype" , 21)
      , ("do" , 22)
      , ("else" , 23)
      , ("end" , 24)
      , ("eqtype" , 25)
      , ("exception" , 26)
      , ("fn" , 27)
      , ("fun" , 28)
      , ("functor" , 29)
      , ("handle" , 30)
      , ("if" , 31)
      , ("in" , 32)
      , ("include" , 33)
      , ("infix" , 34)
      , ("infixr" , 35)
      , ("let" , 36)
      , ("local" , 37)
      , ("nonfix" , 38)
      , ("of" , 39)
      , ("op" , 40)
      , ("open" , 41)
      , ("orelse" , 42)
      , ("raise" , 43)
      , ("rec" , 44)
      , ("sharing" , 45)
      , ("sig" , 46)
      , ("signature" , 47)
      , ("struct" , 48)
      , ("structure" , 49)
      , ("then" , 50)
      , ("type" , 51)
      , ("val" , 52)
      , ("where" , 53)
      , ("while" , 54)
      , ("with" , 55)
      , ("withtype" , 56)
      , ("{" , 57)
      , ("|" , 58)
      , ("}" , 59)
      ]
  )
  open Internal
  
  val parseTerminalChar = parseTerminal (fn TerminalChar v => SOME v | _ => NONE)
  val parseTerminalFloat = parseTerminal (fn TerminalFloat v => SOME v | _ => NONE)
  val parseTerminalId = parseTerminal (fn TerminalId v => SOME v | _ => NONE)
  val parseTerminalInt = parseTerminal (fn TerminalInt v => SOME v | _ => NONE)
  val parseTerminalString = parseTerminal (fn TerminalString v => SOME v | _ => NONE)
  val parseTerminalTyvar = parseTerminal (fn TerminalTyvar v => SOME v | _ => NONE)
  val parseTerminalWord = parseTerminal (fn TerminalWord v => SOME v | _ => NONE)
    val parseConDummy : con t_dummy = dummy ()
    val parseLabDummy : lab t_dummy = dummy ()
    val parseLongIdDummy : long_id t_dummy = dummy ()
    val parseExpDummy : exp t_dummy = dummy ()
    val parseExpListInnerDummy : exp_list_inner t_dummy = dummy ()
    val parseExpRowDummy : exp_row t_dummy = dummy ()
    val parseMatchDummy : match t_dummy = dummy ()
    val parseMatchArmDummy : match_arm t_dummy = dummy ()
    val parsePatDummy : pat t_dummy = dummy ()
    val parsePatListInnerDummy : pat_list_inner t_dummy = dummy ()
    val parsePatRowDummy : pat_row t_dummy = dummy ()
    val parseTypDummy : typ t_dummy = dummy ()
    val parseTypRowDummy : typ_row t_dummy = dummy ()
    val parseDecDummy : dec t_dummy = dummy ()
    val parseDecListDummy : dec_list t_dummy = dummy ()
    val parseTyVarSeqDummy : ty_var_seq t_dummy = dummy ()
    val parseValBindDummy : val_bind t_dummy = dummy ()
    val parseFunBindDummy : fun_bind t_dummy = dummy ()
    val parseFunMatchDummy : fun_match t_dummy = dummy ()
    val parseTypBindDummy : typ_bind t_dummy = dummy ()
    val parseDatBindDummy : dat_bind t_dummy = dummy ()
    val parseConBindDummy : con_bind t_dummy = dummy ()
    val parseExnBindDummy : exn_bind t_dummy = dummy ()
    val parseStrDummy : str t_dummy = dummy ()
    val parseStrBindDummy : str_bind t_dummy = dummy ()
    val parseSigAnnotDummy : sig_annot t_dummy = dummy ()
    val parseSigExpDummy : sig_exp t_dummy = dummy ()
    val parseTypRefinDummy : typ_refin t_dummy = dummy ()
    val parseSpecDummy : spec t_dummy = dummy ()
    val parseSpecListDummy : spec_list t_dummy = dummy ()
    val parseValDescDummy : val_desc t_dummy = dummy ()
    val parseTypDescDummy : typ_desc t_dummy = dummy ()
    val parseDatDescDummy : dat_desc t_dummy = dummy ()
    val parseConDescDummy : con_desc t_dummy = dummy ()
    val parseExnDescDummy : exn_desc t_dummy = dummy ()
    val parseStrDescDummy : str_desc t_dummy = dummy ()
    val parseProgDummy : prog t_dummy = dummy ()
    val parseProgListDummy : prog_list t_dummy = dummy ()
    val parseFctBindDummy : fct_bind t_dummy = dummy ()
    val parseSigBindDummy : sig_bind t_dummy = dummy ()
  
  val lex = lex
    [ addLexer Terminals.Char.lex TerminalChar
    , addLexer Terminals.Float.lex TerminalFloat
    , addLexer Terminals.Id.lex TerminalId
    , addLexer Terminals.Int.lex TerminalInt
    , addLexer Terminals.String.lex TerminalString
    , addLexer Terminals.Tyvar.lex TerminalTyvar
    , addLexer Terminals.Word.lex TerminalWord
    ]
  
    (* Con *)
    val parseCon =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseString =
            create ConString (
              bind (parseTerminalString) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseChar =
            create ConChar (
              bind (parseTerminalChar) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseFloat =
            create ConFloat (
              bind (parseTerminalFloat) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseWord =
            create ConWord (
              bind (parseTerminalWord) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseInt =
            create ConInt (
              bind (parseTerminalInt) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseString
        , parseChar
        , parseFloat
        , parseWord
        , parseInt
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Lab *)
    val parseLab =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseNum =
            create LabNum (
              bind (parseTerminalInt) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseId =
            create LabId (
              bind (parseTerminalId) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseNum
        , parseId
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* LongId *)
    val parseLongId =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseLongId =
            create LongIdLongId (
              bind (parseTerminalId) (fn v0 =>
              bind (starLongest (
                bind (
                  bind (keyword 6) (fn v3 =>
                  bind (parseTerminalId) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseLongId
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Exp *)
    val parseExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseFn =
            create ExpFn (
              bind (keyword 27) (fn v0 =>
              bind (parseNonterminal (deref parseMatchDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseCase =
            create ExpCase (
              bind (keyword 20) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 39) (fn v2 =>
              bind (parseNonterminal (deref parseMatchDummy)) (fn v3 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseLet =
            create ExpLet (
              bind (keyword 36) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 32) (fn v2 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v3 =>
              bind (starLongest (
                bind (
                  bind (keyword 10) (fn v6 =>
                  bind (parseNonterminal (deref parseExpDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              bind (keyword 24) (fn v5 =>
              return_node ((#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 ])))))))
  
          val parseSeq =
            create ExpSeq (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (plusLongest (
                bind (
                  bind (keyword 10) (fn v4 =>
                  bind (parseNonterminal (deref parseExpDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              bind (keyword 2) (fn v3 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseList =
            create ExpList (
              bind (keyword 13) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseExpListInnerDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 14) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseSelector =
            create ExpSelector (
              bind (keyword 0) (fn v0 =>
              bind (parseNonterminal (deref parseLabDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseRecord =
            create ExpRecord (
              bind (keyword 57) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseExpRowDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 59) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseTuple =
            create ExpTuple (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 4) (fn v2 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v3 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v6 =>
                  bind (parseNonterminal (deref parseExpDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              bind (keyword 2) (fn v5 =>
              return_node ((#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 ])))))))
  
          val parseParens =
            create ExpParens (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 2) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseId =
            create ExpId (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseOpId =
            create ExpOpId (
              bind (keyword 40) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseConst =
            create ExpConst (
              bind (parseNonterminal (deref parseConDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseFn
        , parseCase
        , parseLet
        , parseSeq
        , parseList
        , parseSelector
        , parseRecord
        , parseTuple
        , parseParens
        , parseId
        , parseOpId
        , parseConst
        ]
        end)
  
        val parseLevel8 = fix (fn parseLevel8 =>
        let
          val parseApp =
            create ExpApp (
              bind (parseNonterminal parseLevel8) (fn v0 =>
              bind (parseNonterminal (forget parseAtom)) (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseAtom)
        , parseApp
        ]
        end)
  
        val parseLevel7 = fix (fn parseLevel7 =>
        let
          val parseAnnot =
            create ExpAnnot (
              bind (parseNonterminal parseLevel7) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseLevel8)
        , parseAnnot
        ]
        end)
  
        val parseLevel6 = fix (fn parseLevel6 =>
        let
          val parseHandle =
            create ExpHandle (
              bind (parseNonterminal parseLevel6) (fn v0 =>
              bind (keyword 30) (fn v1 =>
              bind (parseNonterminal (deref parseMatchDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseLevel7)
        , parseHandle
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseWhile =
            create ExpWhile (
              bind (keyword 54) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 22) (fn v2 =>
              bind (parseNonterminal parseLevel5) (fn v3 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseIf =
            create ExpIf (
              bind (keyword 31) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 50) (fn v2 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v3 =>
              bind (keyword 23) (fn v4 =>
              bind (parseNonterminal parseLevel5) (fn v5 =>
              return_node ((#node v1) , (#node v3) , (#node v5)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 ])))))))
  
          val parseAndAlso =
            create ExpAndAlso (
              bind (parseNonterminal parseLevel5) (fn v0 =>
              bind (keyword 18) (fn v1 =>
              bind (parseNonterminal (forget parseLevel6)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseRaise =
            create ExpRaise (
              bind (keyword 43) (fn v0 =>
              bind (parseNonterminal parseLevel5) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseLevel6)
        , parseWhile
        , parseIf
        , parseAndAlso
        , parseRaise
        ]
        end)
  
        val parseLevel4 = fix (fn parseLevel4 =>
        let
          val parseOrElse =
            create ExpOrElse (
              bind (parseNonterminal parseLevel4) (fn v0 =>
              bind (keyword 42) (fn v1 =>
              bind (parseNonterminal (forget parseLevel5)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseLevel5)
        , parseOrElse
        ]
        end)
  
      in
        forget parseLevel4
      end
  
    (* ExpListInner *)
    val parseExpListInner =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseExpListInner =
            create ExpListInnerExpListInner (
              bind (parseNonterminal (deref parseExpDummy)) (fn v0 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v3 =>
                  bind (parseNonterminal (deref parseExpDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseExpListInner
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ExpRow *)
    val parseExpRow =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseExpRow =
            create ExpRowExpRow (
              bind (parseNonterminal (deref parseLabDummy)) (fn v0 =>
              bind (keyword 11) (fn v1 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v2 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v5 =>
                  bind (parseNonterminal (deref parseLabDummy)) (fn v6 =>
                  bind (keyword 11) (fn v7 =>
                  bind (parseNonterminal (deref parseExpDummy)) (fn v8 =>
                  return_node ((#node v6) , (#node v8)) [ annot_add v5 , annot_add v6 , annot_add v7 , annot_add v8 ])))))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ parseExpRow
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Match *)
    val parseMatch =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseMatch =
            create MatchMatch (
              bind (parseNonterminal (deref parseMatchArmDummy)) (fn v0 =>
              bind (starLongest (
                bind (
                  bind (keyword 58) (fn v3 =>
                  bind (parseNonterminal (deref parseMatchArmDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseMatch
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* MatchArm *)
    val parseMatchArm =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseMatchArm =
            create MatchArmMatchArm (
              bind (parseNonterminal (deref parsePatDummy)) (fn v0 =>
              bind (keyword 12) (fn v1 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseMatchArm
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Pat *)
    val parsePat =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseList =
            create PatList (
              bind (keyword 13) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parsePatListInnerDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 14) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseRecord =
            create PatRecord (
              bind (keyword 57) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parsePatRowDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 59) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseTuple =
            create PatTuple (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v1 =>
              bind (keyword 4) (fn v2 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v3 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v6 =>
                  bind (parseNonterminal (deref parsePatDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              bind (keyword 2) (fn v5 =>
              return_node ((#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 ])))))))
  
          val parseParens =
            create PatParens (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v1 =>
              bind (keyword 2) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseOpCon =
            create PatOpCon (
              bind (keyword 40) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parsePatDummy)) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseVar =
            create PatVar (
              bind (parseTerminalId) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseOpVar =
            create PatOpVar (
              bind (keyword 40) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseWildcard =
            create (fn () => PatWildcard) (
              bind (keyword 15) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseConst =
            create PatConst (
              bind (parseNonterminal (deref parseConDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseList
        , parseRecord
        , parseTuple
        , parseParens
        , parseOpCon
        , parseVar
        , parseOpVar
        , parseWildcard
        , parseConst
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseLayered =
            create PatLayered (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 8) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 19) (fn v2 =>
              bind (parseNonterminal parseLevel5) (fn v3 =>
              return_node ((#node v0) , (#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseOpLayered =
            create PatOpLayered (
              bind (keyword 40) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 8) (fn v4 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              bind (keyword 19) (fn v3 =>
              bind (parseNonterminal parseLevel5) (fn v4 =>
              return_node ((#node v1) , (#node v2) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseAnnot =
            create PatAnnot (
              bind (parseNonterminal parseLevel5) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseCon =
            create PatCon (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              bind (parseNonterminal parseLevel5) (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseAtom)
        , parseLayered
        , parseOpLayered
        , parseAnnot
        , parseCon
        ]
        end)
  
      in
        forget parseLevel5
      end
  
    (* PatListInner *)
    val parsePatListInner =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parsePatListInner =
            create PatListInnerPatListInner (
              bind (parseNonterminal (deref parsePatDummy)) (fn v0 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v3 =>
                  bind (parseNonterminal (deref parsePatDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parsePatListInner
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* PatRow *)
    val parsePatRow =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseVar =
            create PatRowVar (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 8) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 19) (fn v4 =>
                  bind (parseNonterminal (deref parsePatDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 4) (fn v5 =>
                  bind (parseNonterminal (deref parsePatRowDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v1) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parsePat =
            create PatRowPat (
              bind (parseNonterminal (deref parseLabDummy)) (fn v0 =>
              bind (keyword 11) (fn v1 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 4) (fn v5 =>
                  bind (parseNonterminal (deref parsePatRowDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseWildcard =
            create (fn () => PatRowWildcard) (
              bind (keyword 7) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
        in either
        [ parseVar
        , parsePat
        , parseWildcard
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Typ *)
    val parseTyp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseRecord =
            create TypRecord (
              bind (keyword 57) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseTypRowDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 59) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseParens =
            create TypParens (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v1 =>
              bind (keyword 2) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseCon =
            create TypCon (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseConAppMulti =
            create TypConAppMulti (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v1 =>
              bind (keyword 4) (fn v2 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v3 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v6 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              bind (keyword 2) (fn v5 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v6 =>
              return_node ((#node v1) , (#node v3) , (#node v4) , (#node v6)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 ]))))))))
  
          val parseVar =
            create TypVar (
              bind (parseTerminalTyvar) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseRecord
        , parseParens
        , parseCon
        , parseConAppMulti
        , parseVar
        ]
        end)
  
        val parseLevel3 = fix (fn parseLevel3 =>
        let
          val parseConApp =
            create TypConApp (
              bind (parseNonterminal parseLevel3) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseAtom)
        , parseConApp
        ]
        end)
  
        val parseLevel2 = fix (fn parseLevel2 =>
        let
          val parseTupleTyp =
            create TypTupleTyp (
              bind (parseNonterminal parseLevel2) (fn v0 =>
              bind (plusLongest (
                bind (
                  bind (keyword 3) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseLevel3)
        , parseTupleTyp
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseArrow =
            create TypArrow (
              bind (parseNonterminal (forget parseLevel2)) (fn v0 =>
              bind (keyword 5) (fn v1 =>
              bind (parseNonterminal parseLevel1) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseLevel2)
        , parseArrow
        ]
        end)
  
      in
        forget parseLevel1
      end
  
    (* TypRow *)
    val parseTypRow =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseTypRow =
            create TypRowTypRow (
              bind (parseNonterminal (deref parseLabDummy)) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v2 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v5 =>
                  bind (parseNonterminal (deref parseLabDummy)) (fn v6 =>
                  bind (keyword 8) (fn v7 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v8 =>
                  return_node ((#node v6) , (#node v8)) [ annot_add v5 , annot_add v6 , annot_add v7 , annot_add v8 ])))))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ parseTypRow
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Dec *)
    val parseDec =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseInfixr =
            create DecInfixr (
              bind (keyword 35) (fn v0 =>
              bind (optionalLongest (
                bind (parseTerminalInt) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (parseTerminalId) (fn v2 =>
              bind (starLongest (
                bind (parseTerminalId) (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v1) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseInfix =
            create DecInfix (
              bind (keyword 34) (fn v0 =>
              bind (optionalLongest (
                bind (parseTerminalInt) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (parseTerminalId) (fn v2 =>
              bind (starLongest (
                bind (parseTerminalId) (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v1) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseNonfix =
            create DecNonfix (
              bind (keyword 38) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (starLongest (
                bind (parseTerminalId) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseOpen =
            create DecOpen (
              bind (keyword 41) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              bind (starLongest (
                bind (parseNonterminal (deref parseLongIdDummy)) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseLocal =
            create DecLocal (
              bind (keyword 37) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 32) (fn v2 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v3 =>
              bind (keyword 24) (fn v4 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseSemicolon =
            create (fn () => DecSemicolon) (
              bind (keyword 10) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseStructure =
            create DecStructure (
              bind (keyword 49) (fn v0 =>
              bind (parseNonterminal (deref parseStrBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseException =
            create DecException (
              bind (keyword 26) (fn v0 =>
              bind (parseNonterminal (deref parseExnBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseAbstype =
            create DecAbstype (
              bind (keyword 16) (fn v0 =>
              bind (parseNonterminal (deref parseDatBindDummy)) (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 56) (fn v4 =>
                  bind (parseNonterminal (deref parseTypBindDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              bind (keyword 55) (fn v3 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v4 =>
              bind (keyword 24) (fn v5 =>
              return_node ((#node v1) , (#node v2) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 ])))))))
  
          val parseDatatypeRepl =
            create DecDatatypeRepl (
              bind (keyword 21) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (keyword 21) (fn v3 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v4 =>
              return_node ((#node v1) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseDatatype =
            create DecDatatype (
              bind (keyword 21) (fn v0 =>
              bind (parseNonterminal (deref parseDatBindDummy)) (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 56) (fn v4 =>
                  bind (parseNonterminal (deref parseTypBindDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseType =
            create DecType (
              bind (keyword 51) (fn v0 =>
              bind (parseNonterminal (deref parseTypBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseFun =
            create DecFun (
              bind (keyword 28) (fn v0 =>
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v1 =>
              bind (parseNonterminal (deref parseFunBindDummy)) (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseVal =
            create DecVal (
              bind (keyword 52) (fn v0 =>
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v1 =>
              bind (parseNonterminal (deref parseValBindDummy)) (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseInfixr
        , parseInfix
        , parseNonfix
        , parseOpen
        , parseLocal
        , parseSemicolon
        , parseStructure
        , parseException
        , parseAbstype
        , parseDatatypeRepl
        , parseDatatype
        , parseType
        , parseFun
        , parseVal
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* DecList *)
    val parseDecList =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseDecList =
            create DecListDecList (
              bind (plusLongest (
                bind (parseNonterminal (deref parseDecDummy)) (fn v1 =>
                return_node (#node v1) [ annot_add v1 ])))
              (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseDecList
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* TyVarSeq *)
    val parseTyVarSeq =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseEmpty =
            create (fn () => TyVarSeqEmpty) (
              return_node () [  ])
  
          val parseMany =
            create TyVarSeqMany (
              bind (keyword 1) (fn v0 =>
              bind (parseTerminalTyvar) (fn v1 =>
              bind (starLongest (
                bind (
                  bind (keyword 4) (fn v4 =>
                  bind (parseTerminalTyvar) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              bind (keyword 2) (fn v3 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseOne =
            create TyVarSeqOne (
              bind (parseTerminalTyvar) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseEmpty
        , parseMany
        , parseOne
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ValBind *)
    val parseValBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseValBind =
            create ValBindValBind (
              bind (parseNonterminal (deref parsePatDummy)) (fn v0 =>
              bind (keyword 11) (fn v1 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v5 =>
                  bind (parseNonterminal (deref parseValBindDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ parseValBind
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseRec =
            create ValBindRec (
              bind (keyword 44) (fn v0 =>
              bind (parseNonterminal parseLevel5) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseAtom)
        , parseRec
        ]
        end)
  
      in
        forget parseLevel5
      end
  
    (* FunBind *)
    val parseFunBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseFunBind =
            create FunBindFunBind (
              bind (parseNonterminal (deref parseFunMatchDummy)) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v3 =>
                  bind (parseNonterminal (deref parseFunBindDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseFunBind
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* FunMatch *)
    val parseFunMatch =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseInfixParen =
            create FunMatchInfixParen (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v1 =>
              bind (parseTerminalId) (fn v2 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v3 =>
              bind (keyword 2) (fn v4 =>
              bind (starLongest (
                bind (parseNonterminal (deref parsePatDummy)) (fn v6 =>
                return_node (#node v6) [ annot_add v6 ])))
              (fn v5 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 8) (fn v8 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v9 =>
                  return_node (#node v9) [ annot_add v8 , annot_add v9 ])))
                (fn v7 =>
                return_node (#node v7) [ annot_add v7 ])))
              (fn v6 =>
              bind (keyword 11) (fn v7 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v8 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 58) (fn v11 =>
                  bind (parseNonterminal (deref parseFunMatchDummy)) (fn v12 =>
                  return_node (#node v12) [ annot_add v11 , annot_add v12 ])))
                (fn v10 =>
                return_node (#node v10) [ annot_add v10 ])))
              (fn v9 =>
              return_node ((#node v1) , (#node v2) , (#node v3) , (#node v5) , (#node v6) , (#node v8) , (#node v9)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 , annot_add v7 , annot_add v8 , annot_add v9 ])))))))))))
  
          val parseInfix =
            create FunMatchInfix (
              bind (parseNonterminal (deref parsePatDummy)) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 8) (fn v5 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              bind (keyword 11) (fn v4 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v5 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 58) (fn v8 =>
                  bind (parseNonterminal (deref parseFunMatchDummy)) (fn v9 =>
                  return_node (#node v9) [ annot_add v8 , annot_add v9 ])))
                (fn v7 =>
                return_node (#node v7) [ annot_add v7 ])))
              (fn v6 =>
              return_node ((#node v0) , (#node v1) , (#node v2) , (#node v3) , (#node v5) , (#node v6)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 ]))))))))
  
          val parseNonfix =
            create FunMatchNonfix (
              bind (optionalLongest (
                bind (keyword 40) (fn v1 =>
                return_node () [ annot_add v1 ])))
              (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v2 =>
              bind (starLongest (
                bind (parseNonterminal (deref parsePatDummy)) (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 8) (fn v6 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              bind (keyword 11) (fn v5 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v6 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 58) (fn v9 =>
                  bind (parseNonterminal (deref parseFunMatchDummy)) (fn v10 =>
                  return_node (#node v10) [ annot_add v9 , annot_add v10 ])))
                (fn v8 =>
                return_node (#node v8) [ annot_add v8 ])))
              (fn v7 =>
              return_node ((#node v0) , (#node v1) , (#node v2) , (#node v3) , (#node v4) , (#node v6) , (#node v7)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 , annot_add v7 ])))))))))
  
        in either
        [ parseInfixParen
        , parseInfix
        , parseNonfix
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* TypBind *)
    val parseTypBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseTypBind =
            create TypBindTypBind (
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v3 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v6 =>
                  bind (parseNonterminal (deref parseTypBindDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              return_node ((#node v0) , (#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
        in either
        [ parseTypBind
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* DatBind *)
    val parseDatBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseDatBind =
            create DatBindDatBind (
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (parseNonterminal (deref parseConBindDummy)) (fn v3 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v6 =>
                  bind (parseNonterminal (deref parseDatBindDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              return_node ((#node v0) , (#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
        in either
        [ parseDatBind
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ConBind *)
    val parseConBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseConBind =
            create ConBindConBind (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 39) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 58) (fn v4 =>
                  bind (parseNonterminal (deref parseConBindDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v0) , (#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseConBind
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ExnBind *)
    val parseExnBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseRepl =
            create ExnBindRepl (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 11) (fn v1 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v5 =>
                  bind (parseNonterminal (deref parseExnBindDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseGen =
            create ExnBindGen (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 39) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v4 =>
                  bind (parseNonterminal (deref parseExnBindDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v0) , (#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseRepl
        , parseGen
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Str *)
    val parseStr =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseLet =
            create StrLet (
              bind (keyword 36) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 32) (fn v2 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v3 =>
              bind (keyword 24) (fn v4 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseFctAppDec =
            create StrFctAppDec (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v2 =>
              bind (keyword 2) (fn v3 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseFctApp =
            create StrFctApp (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v2 =>
              bind (keyword 2) (fn v3 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseStruct =
            create StrStruct (
              bind (keyword 48) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 24) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseId =
            create StrId (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseLet
        , parseFctAppDec
        , parseFctApp
        , parseStruct
        , parseId
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseOpaque =
            create StrOpaque (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 9) (fn v1 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseTransparent =
            create StrTransparent (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseAtom)
        , parseOpaque
        , parseTransparent
        ]
        end)
  
      in
        forget parseLevel1
      end
  
    (* StrBind *)
    val parseStrBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseStrBind =
            create StrBindStrBind (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseSigAnnotDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v3 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v6 =>
                  bind (parseNonterminal (deref parseStrBindDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              return_node ((#node v0) , (#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
        in either
        [ parseStrBind
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* SigAnnot *)
    val parseSigAnnot =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseOpaque =
            create SigAnnotOpaque (
              bind (keyword 9) (fn v0 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseTransparent =
            create SigAnnotTransparent (
              bind (keyword 8) (fn v0 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseOpaque
        , parseTransparent
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* SigExp *)
    val parseSigExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseSig =
            create SigExpSig (
              bind (keyword 46) (fn v0 =>
              bind (parseNonterminal (deref parseSpecListDummy)) (fn v1 =>
              bind (keyword 24) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseId =
            create SigExpId (
              bind (parseTerminalId) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseSig
        , parseId
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseWhere =
            create SigExpWhere (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 53) (fn v1 =>
              bind (keyword 51) (fn v2 =>
              bind (parseNonterminal (deref parseTypRefinDummy)) (fn v3 =>
              return_node ((#node v0) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ (forget parseAtom)
        , parseWhere
        ]
        end)
  
      in
        forget parseLevel1
      end
  
    (* TypRefin *)
    val parseTypRefin =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseTypRefin =
            create TypRefinTypRefin (
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v3 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v6 =>
                  bind (keyword 51) (fn v7 =>
                  bind (parseNonterminal (deref parseTypRefinDummy)) (fn v8 =>
                  return_node (#node v8) [ annot_add v6 , annot_add v7 , annot_add v8 ]))))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              return_node ((#node v0) , (#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
        in either
        [ parseTypRefin
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Spec *)
    val parseSpec =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseIncludeMulti =
            create SpecIncludeMulti (
              bind (keyword 33) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (starLongest (
                bind (parseTerminalId) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseInclude =
            create SpecInclude (
              bind (keyword 33) (fn v0 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseSemicolon =
            create (fn () => SpecSemicolon) (
              bind (keyword 10) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseStructure =
            create SpecStructure (
              bind (keyword 49) (fn v0 =>
              bind (parseNonterminal (deref parseStrDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseException =
            create SpecException (
              bind (keyword 26) (fn v0 =>
              bind (parseNonterminal (deref parseExnDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseDatatypeRepl =
            create SpecDatatypeRepl (
              bind (keyword 21) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (keyword 21) (fn v3 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v4 =>
              return_node ((#node v1) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseDatatype =
            create SpecDatatype (
              bind (keyword 21) (fn v0 =>
              bind (parseNonterminal (deref parseDatDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseTypeAbbrev =
            create SpecTypeAbbrev (
              bind (keyword 51) (fn v0 =>
              bind (parseNonterminal (deref parseTypBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseEqtype =
            create SpecEqtype (
              bind (keyword 25) (fn v0 =>
              bind (parseNonterminal (deref parseTypDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseType =
            create SpecType (
              bind (keyword 51) (fn v0 =>
              bind (parseNonterminal (deref parseTypDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseVal =
            create SpecVal (
              bind (keyword 52) (fn v0 =>
              bind (parseNonterminal (deref parseValDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseIncludeMulti
        , parseInclude
        , parseSemicolon
        , parseStructure
        , parseException
        , parseDatatypeRepl
        , parseDatatype
        , parseTypeAbbrev
        , parseEqtype
        , parseType
        , parseVal
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseSharing =
            create SpecSharing (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 45) (fn v1 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v2 =>
              bind (plusLongest (
                bind (
                  bind (keyword 11) (fn v5 =>
                  bind (parseNonterminal (deref parseLongIdDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseSharingType =
            create SpecSharingType (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 45) (fn v1 =>
              bind (keyword 51) (fn v2 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v3 =>
              bind (plusLongest (
                bind (
                  bind (keyword 11) (fn v6 =>
                  bind (parseNonterminal (deref parseLongIdDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              return_node ((#node v0) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
        in either
        [ (forget parseAtom)
        , parseSharing
        , parseSharingType
        ]
        end)
  
      in
        forget parseLevel1
      end
  
    (* SpecList *)
    val parseSpecList =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseSpecList =
            create SpecListSpecList (
              bind (plusLongest (
                bind (parseNonterminal (deref parseSpecDummy)) (fn v1 =>
                return_node (#node v1) [ annot_add v1 ])))
              (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseSpecList
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ValDesc *)
    val parseValDesc =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseValDesc =
            create ValDescValDesc (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v5 =>
                  bind (parseNonterminal (deref parseValDescDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ parseValDesc
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* TypDesc *)
    val parseTypDesc =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseTypDesc =
            create TypDescTypDesc (
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v4 =>
                  bind (parseNonterminal (deref parseTypDescDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v0) , (#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseTypDesc
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* DatDesc *)
    val parseDatDesc =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseDatDesc =
            create DatDescDatDesc (
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (parseNonterminal (deref parseConDescDummy)) (fn v3 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v6 =>
                  bind (parseNonterminal (deref parseDatDescDummy)) (fn v7 =>
                  return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              return_node ((#node v0) , (#node v1) , (#node v3) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
        in either
        [ parseDatDesc
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ConDesc *)
    val parseConDesc =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseConDesc =
            create ConDescConDesc (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 39) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 58) (fn v4 =>
                  bind (parseNonterminal (deref parseConDescDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v0) , (#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseConDesc
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ExnDesc *)
    val parseExnDesc =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseExnDesc =
            create ExnDescExnDesc (
              bind (parseTerminalId) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 39) (fn v3 =>
                  bind (parseNonterminal (deref parseTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v4 =>
                  bind (parseNonterminal (deref parseExnDescDummy)) (fn v5 =>
                  return_node (#node v5) [ annot_add v4 , annot_add v5 ])))
                (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v0) , (#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseExnDesc
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* StrDesc *)
    val parseStrDesc =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseStrDesc =
            create StrDescStrDesc (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v5 =>
                  bind (parseNonterminal (deref parseStrDescDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ parseStrDesc
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Prog *)
    val parseProg =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseSemicolon =
            create (fn () => ProgSemicolon) (
              bind (keyword 10) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseSignature =
            create ProgSignature (
              bind (keyword 47) (fn v0 =>
              bind (parseNonterminal (deref parseSigBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseFunctor =
            create ProgFunctor (
              bind (keyword 29) (fn v0 =>
              bind (parseNonterminal (deref parseFctBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseDec =
            create ProgDec (
              bind (parseNonterminal (deref parseDecDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseSemicolon
        , parseSignature
        , parseFunctor
        , parseDec
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* ProgList *)
    val parseProgList =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseProgList =
            create ProgListProgList (
              bind (plusLongest (
                bind (parseNonterminal (deref parseProgDummy)) (fn v1 =>
                return_node (#node v1) [ annot_add v1 ])))
              (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseProgList
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* FctBind *)
    val parseFctBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseOpened =
            create FctBindOpened (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseNonterminal (deref parseSpecDummy)) (fn v2 =>
              bind (keyword 2) (fn v3 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseSigAnnotDummy)) (fn v5 =>
                return_node (#node v5) [ annot_add v5 ])))
              (fn v4 =>
              bind (keyword 11) (fn v5 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v6 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v9 =>
                  bind (parseNonterminal (deref parseFctBindDummy)) (fn v10 =>
                  return_node (#node v10) [ annot_add v9 , annot_add v10 ])))
                (fn v8 =>
                return_node (#node v8) [ annot_add v8 ])))
              (fn v7 =>
              return_node ((#node v0) , (#node v2) , (#node v4) , (#node v6) , (#node v7)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 , annot_add v7 ])))))))))
  
          val parsePlain =
            create FctBindPlain (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseTerminalId) (fn v2 =>
              bind (keyword 8) (fn v3 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v4 =>
              bind (keyword 2) (fn v5 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseSigAnnotDummy)) (fn v7 =>
                return_node (#node v7) [ annot_add v7 ])))
              (fn v6 =>
              bind (keyword 11) (fn v7 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v8 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v11 =>
                  bind (parseNonterminal (deref parseFctBindDummy)) (fn v12 =>
                  return_node (#node v12) [ annot_add v11 , annot_add v12 ])))
                (fn v10 =>
                return_node (#node v10) [ annot_add v10 ])))
              (fn v9 =>
              return_node ((#node v0) , (#node v2) , (#node v4) , (#node v6) , (#node v8) , (#node v9)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 , annot_add v7 , annot_add v8 , annot_add v9 ])))))))))))
  
        in either
        [ parseOpened
        , parsePlain
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* SigBind *)
    val parseSigBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseSigBind =
            create SigBindSigBind (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 11) (fn v1 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v2 =>
              bind (optionalLongest (
                bind (
                  bind (keyword 17) (fn v5 =>
                  bind (parseNonterminal (deref parseSigBindDummy)) (fn v6 =>
                  return_node (#node v6) [ annot_add v5 , annot_add v6 ])))
                (fn v4 =>
                return_node (#node v4) [ annot_add v4 ])))
              (fn v3 =>
              return_node ((#node v0) , (#node v2) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
        in either
        [ parseSigBind
        ]
        end)
  
      in
        forget parseAtom
      end
  
  
  val () = set parseConDummy parseCon
  val () = set parseLabDummy parseLab
  val () = set parseLongIdDummy parseLongId
  val () = set parseExpDummy parseExp
  val () = set parseExpListInnerDummy parseExpListInner
  val () = set parseExpRowDummy parseExpRow
  val () = set parseMatchDummy parseMatch
  val () = set parseMatchArmDummy parseMatchArm
  val () = set parsePatDummy parsePat
  val () = set parsePatListInnerDummy parsePatListInner
  val () = set parsePatRowDummy parsePatRow
  val () = set parseTypDummy parseTyp
  val () = set parseTypRowDummy parseTypRow
  val () = set parseDecDummy parseDec
  val () = set parseDecListDummy parseDecList
  val () = set parseTyVarSeqDummy parseTyVarSeq
  val () = set parseValBindDummy parseValBind
  val () = set parseFunBindDummy parseFunBind
  val () = set parseFunMatchDummy parseFunMatch
  val () = set parseTypBindDummy parseTypBind
  val () = set parseDatBindDummy parseDatBind
  val () = set parseConBindDummy parseConBind
  val () = set parseExnBindDummy parseExnBind
  val () = set parseStrDummy parseStr
  val () = set parseStrBindDummy parseStrBind
  val () = set parseSigAnnotDummy parseSigAnnot
  val () = set parseSigExpDummy parseSigExp
  val () = set parseTypRefinDummy parseTypRefin
  val () = set parseSpecDummy parseSpec
  val () = set parseSpecListDummy parseSpecList
  val () = set parseValDescDummy parseValDesc
  val () = set parseTypDescDummy parseTypDesc
  val () = set parseDatDescDummy parseDatDesc
  val () = set parseConDescDummy parseConDesc
  val () = set parseExnDescDummy parseExnDesc
  val () = set parseStrDescDummy parseStrDesc
  val () = set parseProgDummy parseProg
  val () = set parseProgListDummy parseProgList
  val () = set parseFctBindDummy parseFctBind
  val () = set parseSigBindDummy parseSigBind
  
  val parse = parser

end