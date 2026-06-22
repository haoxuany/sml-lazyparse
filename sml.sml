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
  
  val printCon : con -> string
  val printLab : lab -> string
  val printLongId : long_id -> string
  val printExp : exp -> string
  val printExpListInner : exp_list_inner -> string
  val printExpRow : exp_row -> string
  val printMatch : match -> string
  val printMatchArm : match_arm -> string
  val printPat : pat -> string
  val printPatListInner : pat_list_inner -> string
  val printPatRow : pat_row -> string
  val printTyp : typ -> string
  val printTypRow : typ_row -> string
  val printDec : dec -> string
  val printDecList : dec_list -> string
  val printTyVarSeq : ty_var_seq -> string
  val printValBind : val_bind -> string
  val printFunBind : fun_bind -> string
  val printFunMatch : fun_match -> string
  val printTypBind : typ_bind -> string
  val printDatBind : dat_bind -> string
  val printConBind : con_bind -> string
  val printExnBind : exn_bind -> string
  val printStr : str -> string
  val printStrBind : str_bind -> string
  val printSigAnnot : sig_annot -> string
  val printSigExp : sig_exp -> string
  val printTypRefin : typ_refin -> string
  val printSpec : spec -> string
  val printSpecList : spec_list -> string
  val printValDesc : val_desc -> string
  val printTypDesc : typ_desc -> string
  val printDatDesc : dat_desc -> string
  val printConDesc : con_desc -> string
  val printExnDesc : exn_desc -> string
  val printStrDesc : str_desc -> string
  val printProg : prog -> string
  val printProgList : prog_list -> string
  val printFctBind : fct_bind -> string
  val printSigBind : sig_bind -> string
  
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
  
  local
    structure LexInternal = LexInternal (structure Stream = Stream)
    datatype terminal_token = TerminalChar of Terminals.Char.t
    | TerminalFloat of Terminals.Float.t
    | TerminalId of Terminals.Id.t
    | TerminalInt of Terminals.Int.t
    | TerminalString of Terminals.String.t
    | TerminalTyvar of Terminals.Tyvar.t
    | TerminalWord of Terminals.Word.t
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
    structure Parcom = Parcom (
      type token = (int , Trivial.t , terminal_token) LexInternal.token
      val table_size = table_size
      structure Stream = Stream
    )
    open Parcom
    fun keyword k = terminal (fn
      LexInternal.TokenKeyword (k' , sp) => if k = k' then SOME sp else NONE
    | _ => NONE)
    val skipTrivial = optionalLongest (remove (fn
      LexInternal.TokenTrivial _ => true
    | _ => false))
    fun parseTerminal proj = terminal (fn
      LexInternal.TokenOther (v , sp) => (case proj v of SOME t => SOME { node = t , span = sp } | NONE => NONE)
    | _ => NONE)
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
  
  in
  type 'a parser = 'a t_memo
  type token_stream = (int , Trivial.t , terminal_token) LexInternal.token Stream.stream
  
  fun lex s pos = LexInternal.lex s pos keywords Trivial.lex
    [ fn x => case Terminals.Char.lex x of SOME (v , s , p) => SOME (TerminalChar v , s , p) | NONE => NONE
    , fn x => case Terminals.Float.lex x of SOME (v , s , p) => SOME (TerminalFloat v , s , p) | NONE => NONE
    , fn x => case Terminals.Id.lex x of SOME (v , s , p) => SOME (TerminalId v , s , p) | NONE => NONE
    , fn x => case Terminals.Int.lex x of SOME (v , s , p) => SOME (TerminalInt v , s , p) | NONE => NONE
    , fn x => case Terminals.String.lex x of SOME (v , s , p) => SOME (TerminalString v , s , p) | NONE => NONE
    , fn x => case Terminals.Tyvar.lex x of SOME (v , s , p) => SOME (TerminalTyvar v , s , p) | NONE => NONE
    , fn x => case Terminals.Word.lex x of SOME (v , s , p) => SOME (TerminalWord v , s , p) | NONE => NONE
    ]
  
    (* Con *)
    val parseCon =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseInt =
            bind skipTrivial (fn _ =>
            bind (parseTerminalInt) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ConInt v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseWord =
            bind skipTrivial (fn _ =>
            bind (parseTerminalWord) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ConWord v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseFloat =
            bind skipTrivial (fn _ =>
            bind (parseTerminalFloat) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ConFloat v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseChar =
            bind skipTrivial (fn _ =>
            bind (parseTerminalChar) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ConChar v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseString =
            bind skipTrivial (fn _ =>
            bind (parseTerminalString) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ConString v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
        in either
        [ parseInt
        , parseWord
        , parseFloat
        , parseChar
        , parseString
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
          val parseId =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            return
              { node = LabId v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseNum =
            bind skipTrivial (fn _ =>
            bind (parseTerminalInt) (fn v0 as { span = v0_span , ... } =>
            return
              { node = LabNum v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
        in either
        [ parseId
        , parseNum
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 6) (fn _ => bind skipTrivial (fn _ => bind (parseTerminalId) (fn v0 => return v0)))))) (fn v1 =>
            return
              { node = LongIdLongId (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })))
  
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
          val parseConst =
            bind (deref parseConDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ExpConst v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
          val parseOpId =
            bind skipTrivial (fn _ =>
            bind (keyword 40) (fn v0 =>
            bind (deref parseLongIdDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ExpOpId v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseId =
            bind (deref parseLongIdDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ExpId v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
          val parseParens =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parseExpDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v2 =>
            return
              { node = ExpParens v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseTuple =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parseExpDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 4) (fn v2 =>
            bind (deref parseExpDummy) (fn v3 as { span = v3_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parseExpDummy) (fn v0 => return v0))))) (fn v4 =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v5 =>
            return
              { node = ExpTuple (v1 , v3 , v4)
              , span = Annot.span (#start v0) (#finish v5)
              })))))))))
  
          val parseRecord =
            bind skipTrivial (fn _ =>
            bind (keyword 57) (fn v0 =>
            bind (optionalLongest (bind (deref parseExpRowDummy) (fn v0 => return v0))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 59) (fn v2 =>
            return
              { node = ExpRecord v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseSelector =
            bind skipTrivial (fn _ =>
            bind (keyword 0) (fn v0 =>
            bind (deref parseLabDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ExpSelector v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseList =
            bind skipTrivial (fn _ =>
            bind (keyword 13) (fn v0 =>
            bind (optionalLongest (bind (deref parseExpListInnerDummy) (fn v0 => return v0))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 14) (fn v2 =>
            return
              { node = ExpList v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseSeq =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parseExpDummy) (fn v1 as { span = v1_span , ... } =>
            bind (plusLongest (bind skipTrivial (fn _ => bind (keyword 10) (fn _ => bind (deref parseExpDummy) (fn v0 => return v0))))) (fn v2 =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v3 =>
            return
              { node = ExpSeq (v1 , v2)
              , span = Annot.span (#start v0) (#finish v3)
              }))))))
  
          val parseLet =
            bind skipTrivial (fn _ =>
            bind (keyword 36) (fn v0 =>
            bind (deref parseDecListDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 32) (fn v2 =>
            bind (deref parseExpDummy) (fn v3 as { span = v3_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 10) (fn _ => bind (deref parseExpDummy) (fn v0 => return v0))))) (fn v4 =>
            bind skipTrivial (fn _ =>
            bind (keyword 24) (fn v5 =>
            return
              { node = ExpLet (v1 , v3 , v4)
              , span = Annot.span (#start v0) (#finish v5)
              })))))))))
  
          val parseCase =
            bind skipTrivial (fn _ =>
            bind (keyword 20) (fn v0 =>
            bind (deref parseExpDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 39) (fn v2 =>
            bind (deref parseMatchDummy) (fn v3 as { span = v3_span , ... } =>
            return
              { node = ExpCase (v1 , v3)
              , span = Annot.span (#start v0) (#finish v3_span)
              }))))))
  
          val parseFn =
            bind skipTrivial (fn _ =>
            bind (keyword 27) (fn v0 =>
            bind (deref parseMatchDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ExpFn v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
        in either
        [ parseConst
        , parseOpId
        , parseId
        , parseParens
        , parseTuple
        , parseRecord
        , parseSelector
        , parseList
        , parseSeq
        , parseLet
        , parseCase
        , parseFn
        ]
        end)
  
        val parseLevel8 = fix (fn parseLevel8 =>
        let
          val parseApp =
            bind parseLevel8 (fn v0 as { span = v0_span , ... } =>
            bind (forget parseAtom) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ExpApp (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v1_span)
              }))
  
        in either
        [ (forget parseAtom)
        , parseApp
        ]
        end)
  
        val parseLevel7 = fix (fn parseLevel7 =>
        let
          val parseAnnot =
            bind parseLevel7 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v1 =>
            bind (deref parseTypDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpAnnot (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
        in either
        [ (forget parseLevel8)
        , parseAnnot
        ]
        end)
  
        val parseLevel6 = fix (fn parseLevel6 =>
        let
          val parseHandle =
            bind parseLevel6 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 30) (fn v1 =>
            bind (deref parseMatchDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpHandle (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
        in either
        [ (forget parseLevel7)
        , parseHandle
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseRaise =
            bind skipTrivial (fn _ =>
            bind (keyword 43) (fn v0 =>
            bind (forget parseLevel6) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ExpRaise v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseAndAlso =
            bind parseLevel5 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 18) (fn v1 =>
            bind (forget parseLevel6) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpAndAlso (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
          val parseIf =
            bind skipTrivial (fn _ =>
            bind (keyword 31) (fn v0 =>
            bind (forget parseLevel6) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 50) (fn v2 =>
            bind (forget parseLevel6) (fn v3 as { span = v3_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 23) (fn v4 =>
            bind parseLevel5 (fn v5 as { span = v5_span , ... } =>
            return
              { node = ExpIf (v1 , v3 , v5)
              , span = Annot.span (#start v0) (#finish v5_span)
              })))))))))
  
          val parseWhile =
            bind skipTrivial (fn _ =>
            bind (keyword 54) (fn v0 =>
            bind (forget parseLevel6) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 22) (fn v2 =>
            bind parseLevel5 (fn v3 as { span = v3_span , ... } =>
            return
              { node = ExpWhile (v1 , v3)
              , span = Annot.span (#start v0) (#finish v3_span)
              }))))))
  
        in either
        [ (forget parseLevel6)
        , parseRaise
        , parseAndAlso
        , parseIf
        , parseWhile
        ]
        end)
  
        val parseLevel4 = fix (fn parseLevel4 =>
        let
          val parseOrElse =
            bind parseLevel4 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 42) (fn v1 =>
            bind (forget parseLevel5) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpOrElse (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
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
            bind (deref parseExpDummy) (fn v0 as { span = v0_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parseExpDummy) (fn v0 => return v0))))) (fn v1 =>
            return
              { node = ExpListInnerExpListInner (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
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
            bind (deref parseLabDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v1 =>
            bind (deref parseExpDummy) (fn v2 as { span = v2_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parseLabDummy) (fn v0 => bind skipTrivial (fn _ => bind (keyword 11) (fn _ => bind (deref parseExpDummy) (fn v1 => return (v0 , v1))))))))) (fn v3 =>
            return
              { node = ExpRowExpRow (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              })))))
  
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
            bind (deref parseMatchArmDummy) (fn v0 as { span = v0_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 58) (fn _ => bind (deref parseMatchArmDummy) (fn v0 => return v0))))) (fn v1 =>
            return
              { node = MatchMatch (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
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
            bind (deref parsePatDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 12) (fn v1 =>
            bind (deref parseExpDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = MatchArmMatchArm (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
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
          val parseConst =
            bind (deref parseConDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = PatConst v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
          val parseWildcard =
            bind skipTrivial (fn _ =>
            bind (keyword 15) (fn v0 =>
            return
              { node = PatWildcard
              , span = Annot.span (#start v0) (#finish v0)
              }))
  
          val parseOpVar =
            bind skipTrivial (fn _ =>
            bind (keyword 40) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            return
              { node = PatOpVar v1
              , span = Annot.span (#start v0) (#finish v1_span)
              }))))
  
          val parseVar =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            return
              { node = PatVar v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseOpCon =
            bind skipTrivial (fn _ =>
            bind (keyword 40) (fn v0 =>
            bind (deref parseLongIdDummy) (fn v1 as { span = v1_span , ... } =>
            bind (optionalLongest (bind (deref parsePatDummy) (fn v0 => return v0))) (fn v2 =>
            return
              { node = PatOpCon (v1 , v2)
              , span = Annot.span (#start v0) (#finish v1_span)
              }))))
  
          val parseParens =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parsePatDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v2 =>
            return
              { node = PatParens v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseTuple =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parsePatDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 4) (fn v2 =>
            bind (deref parsePatDummy) (fn v3 as { span = v3_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parsePatDummy) (fn v0 => return v0))))) (fn v4 =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v5 =>
            return
              { node = PatTuple (v1 , v3 , v4)
              , span = Annot.span (#start v0) (#finish v5)
              })))))))))
  
          val parseRecord =
            bind skipTrivial (fn _ =>
            bind (keyword 57) (fn v0 =>
            bind (optionalLongest (bind (deref parsePatRowDummy) (fn v0 => return v0))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 59) (fn v2 =>
            return
              { node = PatRecord v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseList =
            bind skipTrivial (fn _ =>
            bind (keyword 13) (fn v0 =>
            bind (optionalLongest (bind (deref parsePatListInnerDummy) (fn v0 => return v0))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 14) (fn v2 =>
            return
              { node = PatList v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
        in either
        [ parseConst
        , parseWildcard
        , parseOpVar
        , parseVar
        , parseOpCon
        , parseParens
        , parseTuple
        , parseRecord
        , parseList
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseCon =
            bind (deref parseLongIdDummy) (fn v0 as { span = v0_span , ... } =>
            bind (forget parseAtom) (fn v1 as { span = v1_span , ... } =>
            return
              { node = PatCon (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v1_span)
              }))
  
          val parseAnnot =
            bind parseLevel5 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v1 =>
            bind (deref parseTypDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = PatAnnot (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
          val parseOpLayered =
            bind skipTrivial (fn _ =>
            bind (keyword 40) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v2 =>
            bind skipTrivial (fn _ =>
            bind (keyword 19) (fn v3 =>
            bind (forget parseAtom) (fn v4 as { span = v4_span , ... } =>
            return
              { node = PatOpLayered (v1 , v2 , v4)
              , span = Annot.span (#start v0) (#finish v4_span)
              }))))))))
  
          val parseLayered =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 19) (fn v2 =>
            bind (forget parseAtom) (fn v3 as { span = v3_span , ... } =>
            return
              { node = PatLayered (v0 , v1 , v3)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              }))))))
  
        in either
        [ (forget parseAtom)
        , parseCon
        , parseAnnot
        , parseOpLayered
        , parseLayered
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
            bind (deref parsePatDummy) (fn v0 as { span = v0_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parsePatDummy) (fn v0 => return v0))))) (fn v1 =>
            return
              { node = PatListInnerPatListInner (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
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
          val parseWildcard =
            bind skipTrivial (fn _ =>
            bind (keyword 7) (fn v0 =>
            return
              { node = PatRowWildcard
              , span = Annot.span (#start v0) (#finish v0)
              }))
  
          val parsePat =
            bind (deref parseLabDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v1 =>
            bind (deref parsePatDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parsePatRowDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = PatRowPat (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              })))))
  
          val parseVar =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v1 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 19) (fn _ => bind (deref parsePatDummy) (fn v0 => return v0))))) (fn v2 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parsePatRowDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = PatRowVar (v0 , v1 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })))))
  
        in either
        [ parseWildcard
        , parsePat
        , parseVar
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
          val parseVar =
            bind skipTrivial (fn _ =>
            bind (parseTerminalTyvar) (fn v0 as { span = v0_span , ... } =>
            return
              { node = TypVar v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseConAppMulti =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parseTypDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 4) (fn v2 =>
            bind (deref parseTypDummy) (fn v3 as { span = v3_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v4 =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v5 =>
            bind (deref parseLongIdDummy) (fn v6 as { span = v6_span , ... } =>
            return
              { node = TypConAppMulti (v1 , v3 , v4 , v6)
              , span = Annot.span (#start v0) (#finish v6_span)
              }))))))))))
  
          val parseCon =
            bind (deref parseLongIdDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = TypCon v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
          val parseParens =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parseTypDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v2 =>
            return
              { node = TypParens v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseRecord =
            bind skipTrivial (fn _ =>
            bind (keyword 57) (fn v0 =>
            bind (optionalLongest (bind (deref parseTypRowDummy) (fn v0 => return v0))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 59) (fn v2 =>
            return
              { node = TypRecord v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
        in either
        [ parseVar
        , parseConAppMulti
        , parseCon
        , parseParens
        , parseRecord
        ]
        end)
  
        val parseLevel3 = fix (fn parseLevel3 =>
        let
          val parseConApp =
            bind parseLevel3 (fn v0 as { span = v0_span , ... } =>
            bind (deref parseLongIdDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = TypConApp (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v1_span)
              }))
  
        in either
        [ (forget parseAtom)
        , parseConApp
        ]
        end)
  
        val parseLevel2 = fix (fn parseLevel2 =>
        let
          val parseTupleTyp =
            bind parseLevel2 (fn v0 as { span = v0_span , ... } =>
            bind (plusLongest (bind skipTrivial (fn _ => bind (keyword 3) (fn _ => bind parseLevel2 (fn v0 => return v0))))) (fn v1 =>
            return
              { node = TypTupleTyp (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
        in either
        [ (forget parseLevel3)
        , parseTupleTyp
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseArrow =
            bind (forget parseLevel2) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 5) (fn v1 =>
            bind parseLevel1 (fn v2 as { span = v2_span , ... } =>
            return
              { node = TypArrow (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
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
            bind (deref parseLabDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v1 =>
            bind (deref parseTypDummy) (fn v2 as { span = v2_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind (deref parseLabDummy) (fn v0 => bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v1 => return (v0 , v1))))))))) (fn v3 =>
            return
              { node = TypRowTypRow (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              })))))
  
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
          val parseVal =
            bind skipTrivial (fn _ =>
            bind (keyword 52) (fn v0 =>
            bind (deref parseTyVarSeqDummy) (fn v1 as { span = v1_span , ... } =>
            bind (deref parseValBindDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = DecVal (v1 , v2)
              , span = Annot.span (#start v0) (#finish v2_span)
              }))))
  
          val parseFun =
            bind skipTrivial (fn _ =>
            bind (keyword 28) (fn v0 =>
            bind (deref parseTyVarSeqDummy) (fn v1 as { span = v1_span , ... } =>
            bind (deref parseFunBindDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = DecFun (v1 , v2)
              , span = Annot.span (#start v0) (#finish v2_span)
              }))))
  
          val parseType =
            bind skipTrivial (fn _ =>
            bind (keyword 51) (fn v0 =>
            bind (deref parseTypBindDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = DecType v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseDatatype =
            bind skipTrivial (fn _ =>
            bind (keyword 21) (fn v0 =>
            bind (deref parseDatBindDummy) (fn v1 as { span = v1_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 56) (fn _ => bind (deref parseTypBindDummy) (fn v0 => return v0))))) (fn v2 =>
            return
              { node = DecDatatype (v1 , v2)
              , span = Annot.span (#start v0) (#finish v1_span)
              }))))
  
          val parseDatatypeRepl =
            bind skipTrivial (fn _ =>
            bind (keyword 21) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind skipTrivial (fn _ =>
            bind (keyword 21) (fn v3 =>
            bind (deref parseLongIdDummy) (fn v4 as { span = v4_span , ... } =>
            return
              { node = DecDatatypeRepl (v1 , v4)
              , span = Annot.span (#start v0) (#finish v4_span)
              })))))))))
  
          val parseAbstype =
            bind skipTrivial (fn _ =>
            bind (keyword 16) (fn v0 =>
            bind (deref parseDatBindDummy) (fn v1 as { span = v1_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 56) (fn _ => bind (deref parseTypBindDummy) (fn v0 => return v0))))) (fn v2 =>
            bind skipTrivial (fn _ =>
            bind (keyword 55) (fn v3 =>
            bind (deref parseDecListDummy) (fn v4 as { span = v4_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 24) (fn v5 =>
            return
              { node = DecAbstype (v1 , v2 , v4)
              , span = Annot.span (#start v0) (#finish v5)
              })))))))))
  
          val parseException =
            bind skipTrivial (fn _ =>
            bind (keyword 26) (fn v0 =>
            bind (deref parseExnBindDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = DecException v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseStructure =
            bind skipTrivial (fn _ =>
            bind (keyword 49) (fn v0 =>
            bind (deref parseStrBindDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = DecStructure v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseSemicolon =
            bind skipTrivial (fn _ =>
            bind (keyword 10) (fn v0 =>
            return
              { node = DecSemicolon
              , span = Annot.span (#start v0) (#finish v0)
              }))
  
          val parseLocal =
            bind skipTrivial (fn _ =>
            bind (keyword 37) (fn v0 =>
            bind (deref parseDecListDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 32) (fn v2 =>
            bind (deref parseDecListDummy) (fn v3 as { span = v3_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 24) (fn v4 =>
            return
              { node = DecLocal (v1 , v3)
              , span = Annot.span (#start v0) (#finish v4)
              }))))))))
  
          val parseOpen =
            bind skipTrivial (fn _ =>
            bind (keyword 41) (fn v0 =>
            bind (deref parseLongIdDummy) (fn v1 as { span = v1_span , ... } =>
            bind (starLongest (bind (deref parseLongIdDummy) (fn v0 => return v0))) (fn v2 =>
            return
              { node = DecOpen (v1 , v2)
              , span = Annot.span (#start v0) (#finish v1_span)
              }))))
  
          val parseNonfix =
            bind skipTrivial (fn _ =>
            bind (keyword 38) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (parseTerminalId) (fn v0 => return v0)))) (fn v2 =>
            return
              { node = DecNonfix (v1 , v2)
              , span = Annot.span (#start v0) (#finish v1_span)
              })))))
  
          val parseInfix =
            bind skipTrivial (fn _ =>
            bind (keyword 34) (fn v0 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (parseTerminalInt) (fn v0 => return v0)))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v2 as { span = v2_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (parseTerminalId) (fn v0 => return v0)))) (fn v3 =>
            return
              { node = DecInfix (v1 , v2 , v3)
              , span = Annot.span (#start v0) (#finish v2_span)
              }))))))
  
          val parseInfixr =
            bind skipTrivial (fn _ =>
            bind (keyword 35) (fn v0 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (parseTerminalInt) (fn v0 => return v0)))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v2 as { span = v2_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (parseTerminalId) (fn v0 => return v0)))) (fn v3 =>
            return
              { node = DecInfixr (v1 , v2 , v3)
              , span = Annot.span (#start v0) (#finish v2_span)
              }))))))
  
        in either
        [ parseVal
        , parseFun
        , parseType
        , parseDatatype
        , parseDatatypeRepl
        , parseAbstype
        , parseException
        , parseStructure
        , parseSemicolon
        , parseLocal
        , parseOpen
        , parseNonfix
        , parseInfix
        , parseInfixr
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
            bind (plusLongest (bind (deref parseDecDummy) (fn v0 => return v0))) (fn v0 =>
            return
              { node = DecListDecList v0
              , span = Annot.span Annot.empty Annot.empty
              })
  
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
          val parseOne =
            bind skipTrivial (fn _ =>
            bind (parseTerminalTyvar) (fn v0 as { span = v0_span , ... } =>
            return
              { node = TyVarSeqOne v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseMany =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalTyvar) (fn v1 as { span = v1_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (keyword 4) (fn _ => bind skipTrivial (fn _ => bind (parseTerminalTyvar) (fn v0 => return v0)))))) (fn v2 =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v3 =>
            return
              { node = TyVarSeqMany (v1 , v2)
              , span = Annot.span (#start v0) (#finish v3)
              })))))))
  
          val parseEmpty =
            return
              { node = TyVarSeqEmpty
              , span = Annot.span Annot.empty Annot.empty
              }
  
        in either
        [ parseOne
        , parseMany
        , parseEmpty
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
            bind (deref parsePatDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v1 =>
            bind (deref parseExpDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseValBindDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = ValBindValBind (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              })))))
  
        in either
        [ parseValBind
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseRec =
            bind skipTrivial (fn _ =>
            bind (keyword 44) (fn v0 =>
            bind (forget parseAtom) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ValBindRec v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
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
            bind (deref parseFunMatchDummy) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseFunBindDummy) (fn v0 => return v0))))) (fn v1 =>
            return
              { node = FunBindFunBind (v0 , v1)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
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
          val parseNonfix =
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 40) (fn _ => return ())))) (fn _ =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (deref parsePatDummy) (fn v1 as { span = v1_span , ... } =>
            bind (starLongest (bind (deref parsePatDummy) (fn v0 => return v0))) (fn v2 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v3 =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v4 =>
            bind (deref parseExpDummy) (fn v5 as { span = v5_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 58) (fn _ => bind (deref parseFunMatchDummy) (fn v0 => return v0))))) (fn v6 =>
            return
              { node = FunMatchNonfix (v0 , v1 , v2 , v3 , v5 , v6)
              , span = Annot.span (#start v0_span) (#finish v5_span)
              }))))))))))
  
          val parseInfix =
            bind (deref parsePatDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind (deref parsePatDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v3 =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v4 =>
            bind (deref parseExpDummy) (fn v5 as { span = v5_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 58) (fn _ => bind (deref parseFunMatchDummy) (fn v0 => return v0))))) (fn v6 =>
            return
              { node = FunMatchInfix (v0 , v1 , v2 , v3 , v5 , v6)
              , span = Annot.span (#start v0_span) (#finish v5_span)
              })))))))))
  
          val parseInfixParen =
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v0 =>
            bind (deref parsePatDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v2 as { span = v2_span , ... } =>
            bind (deref parsePatDummy) (fn v3 as { span = v3_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v4 =>
            bind (starLongest (bind (deref parsePatDummy) (fn v0 => return v0))) (fn v5 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 8) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v6 =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v7 =>
            bind (deref parseExpDummy) (fn v8 as { span = v8_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 58) (fn _ => bind (deref parseFunMatchDummy) (fn v0 => return v0))))) (fn v9 =>
            return
              { node = FunMatchInfixParen (v1 , v2 , v3 , v5 , v6 , v8 , v9)
              , span = Annot.span (#start v0) (#finish v8_span)
              }))))))))))))))
  
        in either
        [ parseNonfix
        , parseInfix
        , parseInfixParen
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
            bind (deref parseTyVarSeqDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind (deref parseTypDummy) (fn v3 as { span = v3_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseTypBindDummy) (fn v0 => return v0))))) (fn v4 =>
            return
              { node = TypBindTypBind (v0 , v1 , v3 , v4)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              })))))))
  
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
            bind (deref parseTyVarSeqDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind (deref parseConBindDummy) (fn v3 as { span = v3_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseDatBindDummy) (fn v0 => return v0))))) (fn v4 =>
            return
              { node = DatBindDatBind (v0 , v1 , v3 , v4)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              })))))))
  
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 39) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v1 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 58) (fn _ => bind (deref parseConBindDummy) (fn v0 => return v0))))) (fn v2 =>
            return
              { node = ConBindConBind (v0 , v1 , v2)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))))
  
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
          val parseGen =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 39) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v1 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseExnBindDummy) (fn v0 => return v0))))) (fn v2 =>
            return
              { node = ExnBindGen (v0 , v1 , v2)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))))
  
          val parseRepl =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v1 =>
            bind (deref parseLongIdDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseExnBindDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = ExnBindRepl (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))))
  
        in either
        [ parseGen
        , parseRepl
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
          val parseId =
            bind (deref parseLongIdDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = StrId v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
          val parseStruct =
            bind skipTrivial (fn _ =>
            bind (keyword 48) (fn v0 =>
            bind (deref parseDecListDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 24) (fn v2 =>
            return
              { node = StrStruct v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseFctApp =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v1 =>
            bind (deref parseStrDummy) (fn v2 as { span = v2_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v3 =>
            return
              { node = StrFctApp (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v3)
              })))))))
  
          val parseFctAppDec =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v1 =>
            bind (deref parseDecListDummy) (fn v2 as { span = v2_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v3 =>
            return
              { node = StrFctAppDec (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v3)
              })))))))
  
          val parseLet =
            bind skipTrivial (fn _ =>
            bind (keyword 36) (fn v0 =>
            bind (deref parseDecListDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 32) (fn v2 =>
            bind (deref parseStrDummy) (fn v3 as { span = v3_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 24) (fn v4 =>
            return
              { node = StrLet (v1 , v3)
              , span = Annot.span (#start v0) (#finish v4)
              }))))))))
  
        in either
        [ parseId
        , parseStruct
        , parseFctApp
        , parseFctAppDec
        , parseLet
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseTransparent =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v1 =>
            bind (deref parseSigExpDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = StrTransparent (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
          val parseOpaque =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 9) (fn v1 =>
            bind (deref parseSigExpDummy) (fn v2 as { span = v2_span , ... } =>
            return
              { node = StrOpaque (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
        in either
        [ (forget parseAtom)
        , parseTransparent
        , parseOpaque
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind (deref parseSigAnnotDummy) (fn v0 => return v0))) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind (deref parseStrDummy) (fn v3 as { span = v3_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseStrBindDummy) (fn v0 => return v0))))) (fn v4 =>
            return
              { node = StrBindStrBind (v0 , v1 , v3 , v4)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              })))))))
  
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
          val parseTransparent =
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v0 =>
            bind (deref parseSigExpDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SigAnnotTransparent v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseOpaque =
            bind skipTrivial (fn _ =>
            bind (keyword 9) (fn v0 =>
            bind (deref parseSigExpDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SigAnnotOpaque v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
        in either
        [ parseTransparent
        , parseOpaque
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
          val parseId =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            return
              { node = SigExpId v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
          val parseSig =
            bind skipTrivial (fn _ =>
            bind (keyword 46) (fn v0 =>
            bind (deref parseSpecListDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 24) (fn v2 =>
            return
              { node = SigExpSig v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
        in either
        [ parseId
        , parseSig
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseWhere =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 53) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 51) (fn v2 =>
            bind (deref parseTypRefinDummy) (fn v3 as { span = v3_span , ... } =>
            return
              { node = SigExpWhere (v0 , v3)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              }))))))
  
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
            bind (deref parseTyVarSeqDummy) (fn v0 as { span = v0_span , ... } =>
            bind (deref parseLongIdDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind (deref parseTypDummy) (fn v3 as { span = v3_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind skipTrivial (fn _ => bind (keyword 51) (fn _ => bind (deref parseTypRefinDummy) (fn v0 => return v0))))))) (fn v4 =>
            return
              { node = TypRefinTypRefin (v0 , v1 , v3 , v4)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              }))))))
  
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
          val parseVal =
            bind skipTrivial (fn _ =>
            bind (keyword 52) (fn v0 =>
            bind (deref parseValDescDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecVal v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseType =
            bind skipTrivial (fn _ =>
            bind (keyword 51) (fn v0 =>
            bind (deref parseTypDescDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecType v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseEqtype =
            bind skipTrivial (fn _ =>
            bind (keyword 25) (fn v0 =>
            bind (deref parseTypDescDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecEqtype v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseTypeAbbrev =
            bind skipTrivial (fn _ =>
            bind (keyword 51) (fn v0 =>
            bind (deref parseTypBindDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecTypeAbbrev v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseDatatype =
            bind skipTrivial (fn _ =>
            bind (keyword 21) (fn v0 =>
            bind (deref parseDatDescDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecDatatype v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseDatatypeRepl =
            bind skipTrivial (fn _ =>
            bind (keyword 21) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind skipTrivial (fn _ =>
            bind (keyword 21) (fn v3 =>
            bind (deref parseLongIdDummy) (fn v4 as { span = v4_span , ... } =>
            return
              { node = SpecDatatypeRepl (v1 , v4)
              , span = Annot.span (#start v0) (#finish v4_span)
              })))))))))
  
          val parseException =
            bind skipTrivial (fn _ =>
            bind (keyword 26) (fn v0 =>
            bind (deref parseExnDescDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecException v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseStructure =
            bind skipTrivial (fn _ =>
            bind (keyword 49) (fn v0 =>
            bind (deref parseStrDescDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecStructure v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseSemicolon =
            bind skipTrivial (fn _ =>
            bind (keyword 10) (fn v0 =>
            return
              { node = SpecSemicolon
              , span = Annot.span (#start v0) (#finish v0)
              }))
  
          val parseInclude =
            bind skipTrivial (fn _ =>
            bind (keyword 33) (fn v0 =>
            bind (deref parseSigExpDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = SpecInclude v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseIncludeMulti =
            bind skipTrivial (fn _ =>
            bind (keyword 33) (fn v0 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind (starLongest (bind skipTrivial (fn _ => bind (parseTerminalId) (fn v0 => return v0)))) (fn v2 =>
            return
              { node = SpecIncludeMulti (v1 , v2)
              , span = Annot.span (#start v0) (#finish v1_span)
              })))))
  
        in either
        [ parseVal
        , parseType
        , parseEqtype
        , parseTypeAbbrev
        , parseDatatype
        , parseDatatypeRepl
        , parseException
        , parseStructure
        , parseSemicolon
        , parseInclude
        , parseIncludeMulti
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseSharingType =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 45) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (keyword 51) (fn v2 =>
            bind (deref parseLongIdDummy) (fn v3 as { span = v3_span , ... } =>
            bind (plusLongest (bind skipTrivial (fn _ => bind (keyword 11) (fn _ => bind (deref parseLongIdDummy) (fn v0 => return v0))))) (fn v4 =>
            return
              { node = SpecSharingType (v0 , v3 , v4)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              })))))))
  
          val parseSharing =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 45) (fn v1 =>
            bind (deref parseLongIdDummy) (fn v2 as { span = v2_span , ... } =>
            bind (plusLongest (bind skipTrivial (fn _ => bind (keyword 11) (fn _ => bind (deref parseLongIdDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = SpecSharing (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              })))))
  
        in either
        [ (forget parseAtom)
        , parseSharingType
        , parseSharing
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
            bind (plusLongest (bind (deref parseSpecDummy) (fn v0 => return v0))) (fn v0 =>
            return
              { node = SpecListSpecList v0
              , span = Annot.span Annot.empty Annot.empty
              })
  
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v1 =>
            bind (deref parseTypDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseValDescDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = ValDescValDesc (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))))
  
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
            bind (deref parseTyVarSeqDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseTypDescDummy) (fn v0 => return v0))))) (fn v2 =>
            return
              { node = TypDescTypDesc (v0 , v1 , v2)
              , span = Annot.span (#start v0_span) (#finish v1_span)
              }))))
  
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
            bind (deref parseTyVarSeqDummy) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v2 =>
            bind (deref parseConDescDummy) (fn v3 as { span = v3_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseDatDescDummy) (fn v0 => return v0))))) (fn v4 =>
            return
              { node = DatDescDatDesc (v0 , v1 , v3 , v4)
              , span = Annot.span (#start v0_span) (#finish v3_span)
              })))))))
  
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 39) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v1 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 58) (fn _ => bind (deref parseConDescDummy) (fn v0 => return v0))))) (fn v2 =>
            return
              { node = ConDescConDesc (v0 , v1 , v2)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))))
  
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 39) (fn _ => bind (deref parseTypDummy) (fn v0 => return v0))))) (fn v1 =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseExnDescDummy) (fn v0 => return v0))))) (fn v2 =>
            return
              { node = ExnDescExnDesc (v0 , v1 , v2)
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))))
  
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v1 =>
            bind (deref parseSigExpDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseStrDescDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = StrDescStrDesc (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))))
  
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
          val parseDec =
            bind (deref parseDecDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ProgDec v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
          val parseFunctor =
            bind skipTrivial (fn _ =>
            bind (keyword 29) (fn v0 =>
            bind (deref parseFctBindDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ProgFunctor v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseSignature =
            bind skipTrivial (fn _ =>
            bind (keyword 47) (fn v0 =>
            bind (deref parseSigBindDummy) (fn v1 as { span = v1_span , ... } =>
            return
              { node = ProgSignature v1
              , span = Annot.span (#start v0) (#finish v1_span)
              })))
  
          val parseSemicolon =
            bind skipTrivial (fn _ =>
            bind (keyword 10) (fn v0 =>
            return
              { node = ProgSemicolon
              , span = Annot.span (#start v0) (#finish v0)
              }))
  
        in either
        [ parseDec
        , parseFunctor
        , parseSignature
        , parseSemicolon
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
            bind (plusLongest (bind (deref parseProgDummy) (fn v0 => return v0))) (fn v0 =>
            return
              { node = ProgListProgList v0
              , span = Annot.span Annot.empty Annot.empty
              })
  
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
          val parsePlain =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v1 =>
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v2 as { span = v2_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v3 =>
            bind (deref parseSigExpDummy) (fn v4 as { span = v4_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v5 =>
            bind (optionalLongest (bind (deref parseSigAnnotDummy) (fn v0 => return v0))) (fn v6 =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v7 =>
            bind (deref parseStrDummy) (fn v8 as { span = v8_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseFctBindDummy) (fn v0 => return v0))))) (fn v9 =>
            return
              { node = FctBindPlain (v0 , v2 , v4 , v6 , v8 , v9)
              , span = Annot.span (#start v0_span) (#finish v8_span)
              }))))))))))))))))
  
          val parseOpened =
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v1 =>
            bind (deref parseSpecDummy) (fn v2 as { span = v2_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v3 =>
            bind (optionalLongest (bind (deref parseSigAnnotDummy) (fn v0 => return v0))) (fn v4 =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v5 =>
            bind (deref parseStrDummy) (fn v6 as { span = v6_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseFctBindDummy) (fn v0 => return v0))))) (fn v7 =>
            return
              { node = FctBindOpened (v0 , v2 , v4 , v6 , v7)
              , span = Annot.span (#start v0_span) (#finish v6_span)
              }))))))))))))
  
        in either
        [ parsePlain
        , parseOpened
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
            bind skipTrivial (fn _ =>
            bind (parseTerminalId) (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 11) (fn v1 =>
            bind (deref parseSigExpDummy) (fn v2 as { span = v2_span , ... } =>
            bind (optionalLongest (bind skipTrivial (fn _ => bind (keyword 17) (fn _ => bind (deref parseSigBindDummy) (fn v0 => return v0))))) (fn v3 =>
            return
              { node = SigBindSigBind (v0 , v2 , v3)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))))
  
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
  
    fun printCon buf ({ node , span = _ } : con) =
      case node of
        ConInt v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Int.show v0_node) v0_line end
        | ConWord v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Word.show v0_node) v0_line end
        | ConFloat v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Float.show v0_node) v0_line end
        | ConChar v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Char.show v0_node) v0_line end
        | ConString v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.String.show v0_node) v0_line end
    and printLab buf ({ node , span = _ } : lab) =
      case node of
        LabId v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end
        | LabNum v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Int.show v0_node) v0_line end
    and printLongId buf ({ node , span = _ } : long_id) =
      case node of
        LongIdLongId (v0 , v1) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf "." 0;
            let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
            in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end)) v1
          )
    and printExp buf ({ node , span = _ } : exp) =
      case node of
        ExpConst v0 =>
          printCon buf v0
        | ExpOpId v0 =>
          (
          PrintBuffer.push buf "op" 0;
          printLongId buf v0
          )
        | ExpId v0 =>
          printLongId buf v0
        | ExpApp (v0 , v1) =>
          (
          printExp buf v0;
          printExp buf v1
          )
        | ExpParens v0 =>
          (
          PrintBuffer.push buf "(" 0;
          printExp buf v0;
          PrintBuffer.push buf ")" 0
          )
        | ExpTuple (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "(" 0;
          printExp buf v0;
          PrintBuffer.push buf "," 0;
          printExp buf v1;
          List.app (fn v2 =>
            (
            PrintBuffer.push buf "," 0;
            printExp buf v2)) v2;
          PrintBuffer.push buf ")" 0
          )
        | ExpRecord v0 =>
          (
          PrintBuffer.push buf "{" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            printExpRow buf v0);
          PrintBuffer.push buf "}" 0
          )
        | ExpSelector v0 =>
          (
          PrintBuffer.push buf "#" 0;
          printLab buf v0
          )
        | ExpList v0 =>
          (
          PrintBuffer.push buf "[" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            printExpListInner buf v0);
          PrintBuffer.push buf "]" 0
          )
        | ExpSeq (v0 , v1) =>
          (
          PrintBuffer.push buf "(" 0;
          printExp buf v0;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf ";" 0;
            printExp buf v1)) v1;
          PrintBuffer.push buf ")" 0
          )
        | ExpLet (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "let" 0;
          printDecList buf v0;
          PrintBuffer.push buf "in" 0;
          printExp buf v1;
          List.app (fn v2 =>
            (
            PrintBuffer.push buf ";" 0;
            printExp buf v2)) v2;
          PrintBuffer.push buf "end" 0
          )
        | ExpAnnot (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf ":" 0;
          printTyp buf v1
          )
        | ExpRaise v0 =>
          (
          PrintBuffer.push buf "raise" 0;
          printExp buf v0
          )
        | ExpHandle (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "handle" 0;
          printMatch buf v1
          )
        | ExpAndAlso (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "andalso" 0;
          printExp buf v1
          )
        | ExpOrElse (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "orelse" 0;
          printExp buf v1
          )
        | ExpIf (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "if" 0;
          printExp buf v0;
          PrintBuffer.push buf "then" 0;
          printExp buf v1;
          PrintBuffer.push buf "else" 0;
          printExp buf v2
          )
        | ExpWhile (v0 , v1) =>
          (
          PrintBuffer.push buf "while" 0;
          printExp buf v0;
          PrintBuffer.push buf "do" 0;
          printExp buf v1
          )
        | ExpCase (v0 , v1) =>
          (
          PrintBuffer.push buf "case" 0;
          printExp buf v0;
          PrintBuffer.push buf "of" 0;
          printMatch buf v1
          )
        | ExpFn v0 =>
          (
          PrintBuffer.push buf "fn" 0;
          printMatch buf v0
          )
    and printExpListInner buf ({ node , span = _ } : exp_list_inner) =
      case node of
        ExpListInnerExpListInner (v0 , v1) =>
          (
          printExp buf v0;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf "," 0;
            printExp buf v1)) v1
          )
    and printExpRow buf ({ node , span = _ } : exp_row) =
      case node of
        ExpRowExpRow (v0 , v1 , v2) =>
          (
          printLab buf v0;
          PrintBuffer.push buf "=" 0;
          printExp buf v1;
          List.app (fn (v2 , v3) =>
            (
            PrintBuffer.push buf "," 0;
            printLab buf v2;
            PrintBuffer.push buf "=" 0;
            printExp buf v3)) v2
          )
    and printMatch buf ({ node , span = _ } : match) =
      case node of
        MatchMatch (v0 , v1) =>
          (
          printMatchArm buf v0;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf "|" 0;
            printMatchArm buf v1)) v1
          )
    and printMatchArm buf ({ node , span = _ } : match_arm) =
      case node of
        MatchArmMatchArm (v0 , v1) =>
          (
          printPat buf v0;
          PrintBuffer.push buf "=>" 0;
          printExp buf v1
          )
    and printPat buf ({ node , span = _ } : pat) =
      case node of
        PatConst v0 =>
          printCon buf v0
        | PatWildcard =>
          PrintBuffer.push buf "_" 0
        | PatOpVar v0 =>
          (
          PrintBuffer.push buf "op" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end
          )
        | PatVar v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end
        | PatOpCon (v0 , v1) =>
          (
          PrintBuffer.push buf "op" 0;
          printLongId buf v0;
          (case v1 of NONE => ()
          | SOME v1 =>
            printPat buf v1)
          )
        | PatCon (v0 , v1) =>
          (
          printLongId buf v0;
          printPat buf v1
          )
        | PatParens v0 =>
          (
          PrintBuffer.push buf "(" 0;
          printPat buf v0;
          PrintBuffer.push buf ")" 0
          )
        | PatTuple (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "(" 0;
          printPat buf v0;
          PrintBuffer.push buf "," 0;
          printPat buf v1;
          List.app (fn v2 =>
            (
            PrintBuffer.push buf "," 0;
            printPat buf v2)) v2;
          PrintBuffer.push buf ")" 0
          )
        | PatRecord v0 =>
          (
          PrintBuffer.push buf "{" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            printPatRow buf v0);
          PrintBuffer.push buf "}" 0
          )
        | PatList v0 =>
          (
          PrintBuffer.push buf "[" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            printPatListInner buf v0);
          PrintBuffer.push buf "]" 0
          )
        | PatAnnot (v0 , v1) =>
          (
          printPat buf v0;
          PrintBuffer.push buf ":" 0;
          printTyp buf v1
          )
        | PatOpLayered (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "op" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf ":" 0;
            printTyp buf v1));
          PrintBuffer.push buf "as" 0;
          printPat buf v2
          )
        | PatLayered (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf ":" 0;
            printTyp buf v1));
          PrintBuffer.push buf "as" 0;
          printPat buf v2
          )
    and printPatListInner buf ({ node , span = _ } : pat_list_inner) =
      case node of
        PatListInnerPatListInner (v0 , v1) =>
          (
          printPat buf v0;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf "," 0;
            printPat buf v1)) v1
          )
    and printPatRow buf ({ node , span = _ } : pat_row) =
      case node of
        PatRowWildcard =>
          PrintBuffer.push buf "..." 0
        | PatRowPat (v0 , v1 , v2) =>
          (
          printLab buf v0;
          PrintBuffer.push buf "=" 0;
          printPat buf v1;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "," 0;
            printPatRow buf v2))
          )
        | PatRowVar (v0 , v1 , v2 , v3) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf ":" 0;
            printTyp buf v1));
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "as" 0;
            printPat buf v2));
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf "," 0;
            printPatRow buf v3))
          )
    and printTyp buf ({ node , span = _ } : typ) =
      case node of
        TypVar v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Tyvar.show v0_node) v0_line end
        | TypConApp (v0 , v1) =>
          (
          printTyp buf v0;
          printLongId buf v1
          )
        | TypConAppMulti (v0 , v1 , v2 , v3) =>
          (
          PrintBuffer.push buf "(" 0;
          printTyp buf v0;
          PrintBuffer.push buf "," 0;
          printTyp buf v1;
          List.app (fn v2 =>
            (
            PrintBuffer.push buf "," 0;
            printTyp buf v2)) v2;
          PrintBuffer.push buf ")" 0;
          printLongId buf v3
          )
        | TypCon v0 =>
          printLongId buf v0
        | TypParens v0 =>
          (
          PrintBuffer.push buf "(" 0;
          printTyp buf v0;
          PrintBuffer.push buf ")" 0
          )
        | TypArrow (v0 , v1) =>
          (
          printTyp buf v0;
          PrintBuffer.push buf "->" 0;
          printTyp buf v1
          )
        | TypTupleTyp (v0 , v1) =>
          (
          printTyp buf v0;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf "*" 0;
            printTyp buf v1)) v1
          )
        | TypRecord v0 =>
          (
          PrintBuffer.push buf "{" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            printTypRow buf v0);
          PrintBuffer.push buf "}" 0
          )
    and printTypRow buf ({ node , span = _ } : typ_row) =
      case node of
        TypRowTypRow (v0 , v1 , v2) =>
          (
          printLab buf v0;
          PrintBuffer.push buf ":" 0;
          printTyp buf v1;
          List.app (fn (v2 , v3) =>
            (
            PrintBuffer.push buf "," 0;
            printLab buf v2;
            PrintBuffer.push buf ":" 0;
            printTyp buf v3)) v2
          )
    and printDec buf ({ node , span = _ } : dec) =
      case node of
        DecVal (v0 , v1) =>
          (
          PrintBuffer.push buf "val" 0;
          printTyVarSeq buf v0;
          printValBind buf v1
          )
        | DecFun (v0 , v1) =>
          (
          PrintBuffer.push buf "fun" 0;
          printTyVarSeq buf v0;
          printFunBind buf v1
          )
        | DecType v0 =>
          (
          PrintBuffer.push buf "type" 0;
          printTypBind buf v0
          )
        | DecDatatype (v0 , v1) =>
          (
          PrintBuffer.push buf "datatype" 0;
          printDatBind buf v0;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "withtype" 0;
            printTypBind buf v1))
          )
        | DecDatatypeRepl (v0 , v1) =>
          (
          PrintBuffer.push buf "datatype" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "=" 0;
          PrintBuffer.push buf "datatype" 0;
          printLongId buf v1
          )
        | DecAbstype (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "abstype" 0;
          printDatBind buf v0;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "withtype" 0;
            printTypBind buf v1));
          PrintBuffer.push buf "with" 0;
          printDecList buf v2;
          PrintBuffer.push buf "end" 0
          )
        | DecException v0 =>
          (
          PrintBuffer.push buf "exception" 0;
          printExnBind buf v0
          )
        | DecStructure v0 =>
          (
          PrintBuffer.push buf "structure" 0;
          printStrBind buf v0
          )
        | DecSemicolon =>
          PrintBuffer.push buf ";" 0
        | DecLocal (v0 , v1) =>
          (
          PrintBuffer.push buf "local" 0;
          printDecList buf v0;
          PrintBuffer.push buf "in" 0;
          printDecList buf v1;
          PrintBuffer.push buf "end" 0
          )
        | DecOpen (v0 , v1) =>
          (
          PrintBuffer.push buf "open" 0;
          printLongId buf v0;
          List.app (fn v1 =>
            printLongId buf v1) v1
          )
        | DecNonfix (v0 , v1) =>
          (
          PrintBuffer.push buf "nonfix" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          List.app (fn v1 =>
            let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
            in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end) v1
          )
        | DecInfix (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "infix" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
            in PrintBuffer.push buf (Terminals.Int.show v0_node) v0_line end);
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          List.app (fn v2 =>
            let val { node = v2_node , span = { start = { lineno = v2_line , ... } , ... } } = v2
            in PrintBuffer.push buf (Terminals.Id.show v2_node) v2_line end) v2
          )
        | DecInfixr (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "infixr" 0;
          (case v0 of NONE => ()
          | SOME v0 =>
            let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
            in PrintBuffer.push buf (Terminals.Int.show v0_node) v0_line end);
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          List.app (fn v2 =>
            let val { node = v2_node , span = { start = { lineno = v2_line , ... } , ... } } = v2
            in PrintBuffer.push buf (Terminals.Id.show v2_node) v2_line end) v2
          )
    and printDecList buf ({ node , span = _ } : dec_list) =
      case node of
        DecListDecList v0 =>
          List.app (fn v0 =>
            printDec buf v0) v0
    and printTyVarSeq buf ({ node , span = _ } : ty_var_seq) =
      case node of
        TyVarSeqOne v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Tyvar.show v0_node) v0_line end
        | TyVarSeqMany (v0 , v1) =>
          (
          PrintBuffer.push buf "(" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Tyvar.show v0_node) v0_line end;
          List.app (fn v1 =>
            (
            PrintBuffer.push buf "," 0;
            let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
            in PrintBuffer.push buf (Terminals.Tyvar.show v1_node) v1_line end)) v1;
          PrintBuffer.push buf ")" 0
          )
        | TyVarSeqEmpty =>
          ()
    and printValBind buf ({ node , span = _ } : val_bind) =
      case node of
        ValBindValBind (v0 , v1 , v2) =>
          (
          printPat buf v0;
          PrintBuffer.push buf "=" 0;
          printExp buf v1;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printValBind buf v2))
          )
        | ValBindRec v0 =>
          (
          PrintBuffer.push buf "rec" 0;
          printValBind buf v0
          )
    and printFunBind buf ({ node , span = _ } : fun_bind) =
      case node of
        FunBindFunBind (v0 , v1) =>
          (
          printFunMatch buf v0;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "and" 0;
            printFunBind buf v1))
          )
    and printFunMatch buf ({ node , span = _ } : fun_match) =
      case node of
        FunMatchNonfix (v0 , v1 , v2 , v3 , v4 , v5) =>
          (
          PrintBuffer.push buf "op" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          printPat buf v1;
          List.app (fn v2 =>
            printPat buf v2) v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf ":" 0;
            printTyp buf v3));
          PrintBuffer.push buf "=" 0;
          printExp buf v4;
          (case v5 of NONE => ()
          | SOME v5 =>
            (
            PrintBuffer.push buf "|" 0;
            printFunMatch buf v5))
          )
        | FunMatchInfix (v0 , v1 , v2 , v3 , v4 , v5) =>
          (
          printPat buf v0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          printPat buf v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf ":" 0;
            printTyp buf v3));
          PrintBuffer.push buf "=" 0;
          printExp buf v4;
          (case v5 of NONE => ()
          | SOME v5 =>
            (
            PrintBuffer.push buf "|" 0;
            printFunMatch buf v5))
          )
        | FunMatchInfixParen (v0 , v1 , v2 , v3 , v4 , v5 , v6) =>
          (
          PrintBuffer.push buf "(" 0;
          printPat buf v0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          printPat buf v2;
          PrintBuffer.push buf ")" 0;
          List.app (fn v3 =>
            printPat buf v3) v3;
          (case v4 of NONE => ()
          | SOME v4 =>
            (
            PrintBuffer.push buf ":" 0;
            printTyp buf v4));
          PrintBuffer.push buf "=" 0;
          printExp buf v5;
          (case v6 of NONE => ()
          | SOME v6 =>
            (
            PrintBuffer.push buf "|" 0;
            printFunMatch buf v6))
          )
    and printTypBind buf ({ node , span = _ } : typ_bind) =
      case node of
        TypBindTypBind (v0 , v1 , v2 , v3) =>
          (
          printTyVarSeq buf v0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          PrintBuffer.push buf "=" 0;
          printTyp buf v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf "and" 0;
            printTypBind buf v3))
          )
    and printDatBind buf ({ node , span = _ } : dat_bind) =
      case node of
        DatBindDatBind (v0 , v1 , v2 , v3) =>
          (
          printTyVarSeq buf v0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          PrintBuffer.push buf "=" 0;
          printConBind buf v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf "and" 0;
            printDatBind buf v3))
          )
    and printConBind buf ({ node , span = _ } : con_bind) =
      case node of
        ConBindConBind (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "of" 0;
            printTyp buf v1));
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "|" 0;
            printConBind buf v2))
          )
    and printExnBind buf ({ node , span = _ } : exn_bind) =
      case node of
        ExnBindGen (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "of" 0;
            printTyp buf v1));
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printExnBind buf v2))
          )
        | ExnBindRepl (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "=" 0;
          printLongId buf v1;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printExnBind buf v2))
          )
    and printStr buf ({ node , span = _ } : str) =
      case node of
        StrId v0 =>
          printLongId buf v0
        | StrStruct v0 =>
          (
          PrintBuffer.push buf "struct" 0;
          printDecList buf v0;
          PrintBuffer.push buf "end" 0
          )
        | StrTransparent (v0 , v1) =>
          (
          printStr buf v0;
          PrintBuffer.push buf ":" 0;
          printSigExp buf v1
          )
        | StrOpaque (v0 , v1) =>
          (
          printStr buf v0;
          PrintBuffer.push buf ":>" 0;
          printSigExp buf v1
          )
        | StrFctApp (v0 , v1) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "(" 0;
          printStr buf v1;
          PrintBuffer.push buf ")" 0
          )
        | StrFctAppDec (v0 , v1) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "(" 0;
          printDecList buf v1;
          PrintBuffer.push buf ")" 0
          )
        | StrLet (v0 , v1) =>
          (
          PrintBuffer.push buf "let" 0;
          printDecList buf v0;
          PrintBuffer.push buf "in" 0;
          printStr buf v1;
          PrintBuffer.push buf "end" 0
          )
    and printStrBind buf ({ node , span = _ } : str_bind) =
      case node of
        StrBindStrBind (v0 , v1 , v2 , v3) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            printSigAnnot buf v1);
          PrintBuffer.push buf "=" 0;
          printStr buf v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf "and" 0;
            printStrBind buf v3))
          )
    and printSigAnnot buf ({ node , span = _ } : sig_annot) =
      case node of
        SigAnnotTransparent v0 =>
          (
          PrintBuffer.push buf ":" 0;
          printSigExp buf v0
          )
        | SigAnnotOpaque v0 =>
          (
          PrintBuffer.push buf ":>" 0;
          printSigExp buf v0
          )
    and printSigExp buf ({ node , span = _ } : sig_exp) =
      case node of
        SigExpId v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end
        | SigExpSig v0 =>
          (
          PrintBuffer.push buf "sig" 0;
          printSpecList buf v0;
          PrintBuffer.push buf "end" 0
          )
        | SigExpWhere (v0 , v1) =>
          (
          printSigExp buf v0;
          PrintBuffer.push buf "where" 0;
          PrintBuffer.push buf "type" 0;
          printTypRefin buf v1
          )
    and printTypRefin buf ({ node , span = _ } : typ_refin) =
      case node of
        TypRefinTypRefin (v0 , v1 , v2 , v3) =>
          (
          printTyVarSeq buf v0;
          printLongId buf v1;
          PrintBuffer.push buf "=" 0;
          printTyp buf v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf "and" 0;
            PrintBuffer.push buf "type" 0;
            printTypRefin buf v3))
          )
    and printSpec buf ({ node , span = _ } : spec) =
      case node of
        SpecVal v0 =>
          (
          PrintBuffer.push buf "val" 0;
          printValDesc buf v0
          )
        | SpecType v0 =>
          (
          PrintBuffer.push buf "type" 0;
          printTypDesc buf v0
          )
        | SpecEqtype v0 =>
          (
          PrintBuffer.push buf "eqtype" 0;
          printTypDesc buf v0
          )
        | SpecTypeAbbrev v0 =>
          (
          PrintBuffer.push buf "type" 0;
          printTypBind buf v0
          )
        | SpecDatatype v0 =>
          (
          PrintBuffer.push buf "datatype" 0;
          printDatDesc buf v0
          )
        | SpecDatatypeRepl (v0 , v1) =>
          (
          PrintBuffer.push buf "datatype" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "=" 0;
          PrintBuffer.push buf "datatype" 0;
          printLongId buf v1
          )
        | SpecException v0 =>
          (
          PrintBuffer.push buf "exception" 0;
          printExnDesc buf v0
          )
        | SpecStructure v0 =>
          (
          PrintBuffer.push buf "structure" 0;
          printStrDesc buf v0
          )
        | SpecSemicolon =>
          PrintBuffer.push buf ";" 0
        | SpecInclude v0 =>
          (
          PrintBuffer.push buf "include" 0;
          printSigExp buf v0
          )
        | SpecIncludeMulti (v0 , v1) =>
          (
          PrintBuffer.push buf "include" 0;
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          List.app (fn v1 =>
            let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
            in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end) v1
          )
        | SpecSharingType (v0 , v1 , v2) =>
          (
          printSpec buf v0;
          PrintBuffer.push buf "sharing" 0;
          PrintBuffer.push buf "type" 0;
          printLongId buf v1;
          List.app (fn v2 =>
            (
            PrintBuffer.push buf "=" 0;
            printLongId buf v2)) v2
          )
        | SpecSharing (v0 , v1 , v2) =>
          (
          printSpec buf v0;
          PrintBuffer.push buf "sharing" 0;
          printLongId buf v1;
          List.app (fn v2 =>
            (
            PrintBuffer.push buf "=" 0;
            printLongId buf v2)) v2
          )
    and printSpecList buf ({ node , span = _ } : spec_list) =
      case node of
        SpecListSpecList v0 =>
          List.app (fn v0 =>
            printSpec buf v0) v0
    and printValDesc buf ({ node , span = _ } : val_desc) =
      case node of
        ValDescValDesc (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf ":" 0;
          printTyp buf v1;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printValDesc buf v2))
          )
    and printTypDesc buf ({ node , span = _ } : typ_desc) =
      case node of
        TypDescTypDesc (v0 , v1 , v2) =>
          (
          printTyVarSeq buf v0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printTypDesc buf v2))
          )
    and printDatDesc buf ({ node , span = _ } : dat_desc) =
      case node of
        DatDescDatDesc (v0 , v1 , v2 , v3) =>
          (
          printTyVarSeq buf v0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          PrintBuffer.push buf "=" 0;
          printConDesc buf v2;
          (case v3 of NONE => ()
          | SOME v3 =>
            (
            PrintBuffer.push buf "and" 0;
            printDatDesc buf v3))
          )
    and printConDesc buf ({ node , span = _ } : con_desc) =
      case node of
        ConDescConDesc (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "of" 0;
            printTyp buf v1));
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "|" 0;
            printConDesc buf v2))
          )
    and printExnDesc buf ({ node , span = _ } : exn_desc) =
      case node of
        ExnDescExnDesc (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          (case v1 of NONE => ()
          | SOME v1 =>
            (
            PrintBuffer.push buf "of" 0;
            printTyp buf v1));
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printExnDesc buf v2))
          )
    and printStrDesc buf ({ node , span = _ } : str_desc) =
      case node of
        StrDescStrDesc (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf ":" 0;
          printSigExp buf v1;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printStrDesc buf v2))
          )
    and printProg buf ({ node , span = _ } : prog) =
      case node of
        ProgDec v0 =>
          printDec buf v0
        | ProgFunctor v0 =>
          (
          PrintBuffer.push buf "functor" 0;
          printFctBind buf v0
          )
        | ProgSignature v0 =>
          (
          PrintBuffer.push buf "signature" 0;
          printSigBind buf v0
          )
        | ProgSemicolon =>
          PrintBuffer.push buf ";" 0
    and printProgList buf ({ node , span = _ } : prog_list) =
      case node of
        ProgListProgList v0 =>
          List.app (fn v0 =>
            printProg buf v0) v0
    and printFctBind buf ({ node , span = _ } : fct_bind) =
      case node of
        FctBindPlain (v0 , v1 , v2 , v3 , v4 , v5) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "(" 0;
          let val { node = v1_node , span = { start = { lineno = v1_line , ... } , ... } } = v1
          in PrintBuffer.push buf (Terminals.Id.show v1_node) v1_line end;
          PrintBuffer.push buf ":" 0;
          printSigExp buf v2;
          PrintBuffer.push buf ")" 0;
          (case v3 of NONE => ()
          | SOME v3 =>
            printSigAnnot buf v3);
          PrintBuffer.push buf "=" 0;
          printStr buf v4;
          (case v5 of NONE => ()
          | SOME v5 =>
            (
            PrintBuffer.push buf "and" 0;
            printFctBind buf v5))
          )
        | FctBindOpened (v0 , v1 , v2 , v3 , v4) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "(" 0;
          printSpec buf v1;
          PrintBuffer.push buf ")" 0;
          (case v2 of NONE => ()
          | SOME v2 =>
            printSigAnnot buf v2);
          PrintBuffer.push buf "=" 0;
          printStr buf v3;
          (case v4 of NONE => ()
          | SOME v4 =>
            (
            PrintBuffer.push buf "and" 0;
            printFctBind buf v4))
          )
    and printSigBind buf ({ node , span = _ } : sig_bind) =
      case node of
        SigBindSigBind (v0 , v1 , v2) =>
          (
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Id.show v0_node) v0_line end;
          PrintBuffer.push buf "=" 0;
          printSigExp buf v1;
          (case v2 of NONE => ()
          | SOME v2 =>
            (
            PrintBuffer.push buf "and" 0;
            printSigBind buf v2))
          )
  
  fun print f v = let val buf = PrintBuffer.empty () in f buf v; PrintBuffer.toString buf end
  val printCon = print printCon
  val printLab = print printLab
  val printLongId = print printLongId
  val printExp = print printExp
  val printExpListInner = print printExpListInner
  val printExpRow = print printExpRow
  val printMatch = print printMatch
  val printMatchArm = print printMatchArm
  val printPat = print printPat
  val printPatListInner = print printPatListInner
  val printPatRow = print printPatRow
  val printTyp = print printTyp
  val printTypRow = print printTypRow
  val printDec = print printDec
  val printDecList = print printDecList
  val printTyVarSeq = print printTyVarSeq
  val printValBind = print printValBind
  val printFunBind = print printFunBind
  val printFunMatch = print printFunMatch
  val printTypBind = print printTypBind
  val printDatBind = print printDatBind
  val printConBind = print printConBind
  val printExnBind = print printExnBind
  val printStr = print printStr
  val printStrBind = print printStrBind
  val printSigAnnot = print printSigAnnot
  val printSigExp = print printSigExp
  val printTypRefin = print printTypRefin
  val printSpec = print printSpec
  val printSpecList = print printSpecList
  val printValDesc = print printValDesc
  val printTypDesc = print printTypDesc
  val printDatDesc = print printDatDesc
  val printConDesc = print printConDesc
  val printExnDesc = print printExnDesc
  val printStrDesc = print printStrDesc
  val printProg = print printProg
  val printProgList = print printProgList
  val printFctBind = print printFctBind
  val printSigBind = print printSigBind
  
  val parse = parser
  
  end

end