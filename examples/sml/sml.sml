signature SML_AST = sig
  type 'a annot = { node : 'a , span : Annot.span }
  
  (* terminals *)
  type char
  type float
  type id
  type int
  type string
  type tyvar
  type word
  
  (* nonterminals *)
  datatype con' = ConInt of int annot
    | ConWord of word annot
    | ConFloat of float annot
    | ConChar of char annot
    | ConString of string annot
  and lab' = LabId of id annot
    | LabNum of int annot
  and long_id' = LongIdLongId of id annot * id annot list
  and atom_exp' = AtomExpConst of con
    | AtomExpOpId of long_id
    | AtomExpId of long_id
    | AtomExpParens of exp
    | AtomExpTuple of exp * exp * exp list
    | AtomExpRecord of exp_row option
    | AtomExpSelector of lab
    | AtomExpList of exp_list_inner option
    | AtomExpSeq of exp * exp list
    | AtomExpLet of dec_list * exp * exp list
  and exp' = ExpApp of atom_exp list
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
    | PatOpVar of id annot
    | PatVar of id annot
    | PatOpCon of long_id * pat option
    | PatCon of long_id * pat
    | PatParens of pat
    | PatTuple of pat * pat * pat list
    | PatRecord of pat_row option
    | PatList of pat_list_inner option
    | PatAnnot of pat * typ
    | PatOpLayered of id annot * typ option * pat
    | PatLayered of id annot * typ option * pat
  and pat_list_inner' = PatListInnerPatListInner of pat * pat list
  and pat_row' = PatRowWildcard
    | PatRowPat of lab * pat * pat_row option
    | PatRowVar of id annot * typ option * pat option * pat_row option
  and atom_typ' = AtomTypVar of tyvar annot
    | AtomTypConApp of atom_typ * long_id
    | AtomTypConAppMulti of typ * typ * typ list * long_id
    | AtomTypCon of long_id
    | AtomTypParens of typ
    | AtomTypRecord of typ_row option
  and typ' = TypInner of atom_typ
    | TypTupleTyp of atom_typ * atom_typ list
    | TypArrow of typ * typ
  and typ_row' = TypRowTypRow of lab * typ * (lab * typ) list
  and dec' = DecVal of ty_var_seq * val_bind
    | DecFun of ty_var_seq * fun_bind
    | DecType of typ_bind
    | DecDatatype of dat_bind * typ_bind option
    | DecDatatypeRepl of id annot * long_id
    | DecAbstype of dat_bind * typ_bind option * dec_list
    | DecException of exn_bind
    | DecStructure of str_bind
    | DecSemicolon
    | DecLocal of dec_list * dec_list
    | DecOpen of long_id * long_id list
    | DecNonfix of id annot * id annot list
    | DecInfix of int annot option * id annot * id annot list
    | DecInfixr of int annot option * id annot * id annot list
  and dec_list' = DecListDecList of dec list
  and ty_var_seq' = TyVarSeqOne of tyvar annot
    | TyVarSeqMany of tyvar annot * tyvar annot list
    | TyVarSeqEmpty
  and val_bind' = ValBindValBind of pat * exp * val_bind option
    | ValBindRec of val_bind
  and fun_bind' = FunBindFunBind of fun_match * fun_bind option
  and fun_match' = FunMatchNonfix of id annot * pat * pat list * typ option * exp * fun_match option
    | FunMatchInfix of pat * id annot * pat * typ option * exp * fun_match option
    | FunMatchInfixParen of pat * id annot * pat * pat list * typ option * exp * fun_match option
  and typ_bind' = TypBindTypBind of ty_var_seq * id annot * typ * typ_bind option
  and dat_bind' = DatBindDatBind of ty_var_seq * id annot * con_bind * dat_bind option
  and con_bind' = ConBindConBind of id annot * typ option * con_bind option
  and exn_bind' = ExnBindGen of id annot * typ option * exn_bind option
    | ExnBindRepl of id annot * long_id * exn_bind option
  and str' = StrId of long_id
    | StrStruct of dec_list
    | StrTransparent of str * sig_exp
    | StrOpaque of str * sig_exp
    | StrFctApp of id annot * str
    | StrFctAppDec of id annot * dec_list
    | StrLet of dec_list * str
  and str_bind' = StrBindStrBind of id annot * sig_annot option * str * str_bind option
  and sig_annot' = SigAnnotTransparent of sig_exp
    | SigAnnotOpaque of sig_exp
  and sig_exp' = SigExpId of id annot
    | SigExpSig of spec_list
    | SigExpWhere of sig_exp * typ_refin
  and typ_refin' = TypRefinTypRefin of ty_var_seq * long_id * typ * typ_refin option
  and spec' = SpecVal of val_desc
    | SpecType of typ_desc
    | SpecEqtype of typ_desc
    | SpecTypeAbbrev of typ_bind
    | SpecDatatype of dat_desc
    | SpecDatatypeRepl of id annot * long_id
    | SpecException of exn_desc
    | SpecStructure of str_desc
    | SpecSemicolon
    | SpecInclude of sig_exp
    | SpecIncludeMulti of id annot * id annot list
    | SpecSharingType of spec * long_id * long_id list
    | SpecSharing of spec * long_id * long_id list
  and spec_list' = SpecListSpecList of spec list
  and val_desc' = ValDescValDesc of id annot * typ * val_desc option
  and typ_desc' = TypDescTypDesc of ty_var_seq * id annot * typ_desc option
  and dat_desc' = DatDescDatDesc of ty_var_seq * id annot * con_desc * dat_desc option
  and con_desc' = ConDescConDesc of id annot * typ option * con_desc option
  and exn_desc' = ExnDescExnDesc of id annot * typ option * exn_desc option
  and str_desc' = StrDescStrDesc of id annot * sig_exp * str_desc option
  and prog' = ProgDec of dec
    | ProgFunctor of fct_bind
    | ProgSignature of sig_bind
    | ProgSemicolon
  and prog_list' = ProgListProgList of prog list
  and fct_bind' = FctBindPlain of id annot * id annot * sig_exp * sig_annot option * str * fct_bind option
    | FctBindOpened of id annot * spec * sig_annot option * str * fct_bind option
  and sig_bind' = SigBindSigBind of id annot * sig_exp * sig_bind option
  withtype con = con' annot
  and lab = lab' annot
  and long_id = long_id' annot
  and atom_exp = atom_exp' annot
  and exp = exp' annot
  and exp_list_inner = exp_list_inner' annot
  and exp_row = exp_row' annot
  and match = match' annot
  and match_arm = match_arm' annot
  and pat = pat' annot
  and pat_list_inner = pat_list_inner' annot
  and pat_row = pat_row' annot
  and atom_typ = atom_typ' annot
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
  
end

functor SmlParser (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Char : TERMINAL
    structure Float : TERMINAL
    structure Id : TERMINAL
    structure Int : TERMINAL
    structure String : TERMINAL
    structure Tyvar : TERMINAL
    structure Word : TERMINAL
  end
) :>
sig
  include SML_AST
  where type char = Terminals.Char.t
  where type float = Terminals.Float.t
  where type id = Terminals.Id.t
  where type int = Terminals.Int.t
  where type string = Terminals.String.t
  where type tyvar = Terminals.Tyvar.t
  where type word = Terminals.Word.t
  
  type 'a parser
  type token_stream
  exception LexError of Char.char * Annot.pos
  val lex : Char.char Stream.stream -> Annot.pos -> token_stream
  val parseCon : con parser
  val parseLab : lab parser
  val parseLongId : long_id parser
  val parseAtomExp : atom_exp parser
  val parseExp : exp parser
  val parseExpListInner : exp_list_inner parser
  val parseExpRow : exp_row parser
  val parseMatch : match parser
  val parseMatchArm : match_arm parser
  val parsePat : pat parser
  val parsePatListInner : pat_list_inner parser
  val parsePatRow : pat_row parser
  val parseAtomTyp : atom_typ parser
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
  type char = Terminals.Char.t
  type float = Terminals.Float.t
  type id = Terminals.Id.t
  type int = Terminals.Int.t
  type string = Terminals.String.t
  type tyvar = Terminals.Tyvar.t
  type word = Terminals.Word.t
  
  datatype con' = ConInt of int annot
    | ConWord of word annot
    | ConFloat of float annot
    | ConChar of char annot
    | ConString of string annot
  and lab' = LabId of id annot
    | LabNum of int annot
  and long_id' = LongIdLongId of id annot * id annot list
  and atom_exp' = AtomExpConst of con
    | AtomExpOpId of long_id
    | AtomExpId of long_id
    | AtomExpParens of exp
    | AtomExpTuple of exp * exp * exp list
    | AtomExpRecord of exp_row option
    | AtomExpSelector of lab
    | AtomExpList of exp_list_inner option
    | AtomExpSeq of exp * exp list
    | AtomExpLet of dec_list * exp * exp list
  and exp' = ExpApp of atom_exp list
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
    | PatOpVar of id annot
    | PatVar of id annot
    | PatOpCon of long_id * pat option
    | PatCon of long_id * pat
    | PatParens of pat
    | PatTuple of pat * pat * pat list
    | PatRecord of pat_row option
    | PatList of pat_list_inner option
    | PatAnnot of pat * typ
    | PatOpLayered of id annot * typ option * pat
    | PatLayered of id annot * typ option * pat
  and pat_list_inner' = PatListInnerPatListInner of pat * pat list
  and pat_row' = PatRowWildcard
    | PatRowPat of lab * pat * pat_row option
    | PatRowVar of id annot * typ option * pat option * pat_row option
  and atom_typ' = AtomTypVar of tyvar annot
    | AtomTypConApp of atom_typ * long_id
    | AtomTypConAppMulti of typ * typ * typ list * long_id
    | AtomTypCon of long_id
    | AtomTypParens of typ
    | AtomTypRecord of typ_row option
  and typ' = TypInner of atom_typ
    | TypTupleTyp of atom_typ * atom_typ list
    | TypArrow of typ * typ
  and typ_row' = TypRowTypRow of lab * typ * (lab * typ) list
  and dec' = DecVal of ty_var_seq * val_bind
    | DecFun of ty_var_seq * fun_bind
    | DecType of typ_bind
    | DecDatatype of dat_bind * typ_bind option
    | DecDatatypeRepl of id annot * long_id
    | DecAbstype of dat_bind * typ_bind option * dec_list
    | DecException of exn_bind
    | DecStructure of str_bind
    | DecSemicolon
    | DecLocal of dec_list * dec_list
    | DecOpen of long_id * long_id list
    | DecNonfix of id annot * id annot list
    | DecInfix of int annot option * id annot * id annot list
    | DecInfixr of int annot option * id annot * id annot list
  and dec_list' = DecListDecList of dec list
  and ty_var_seq' = TyVarSeqOne of tyvar annot
    | TyVarSeqMany of tyvar annot * tyvar annot list
    | TyVarSeqEmpty
  and val_bind' = ValBindValBind of pat * exp * val_bind option
    | ValBindRec of val_bind
  and fun_bind' = FunBindFunBind of fun_match * fun_bind option
  and fun_match' = FunMatchNonfix of id annot * pat * pat list * typ option * exp * fun_match option
    | FunMatchInfix of pat * id annot * pat * typ option * exp * fun_match option
    | FunMatchInfixParen of pat * id annot * pat * pat list * typ option * exp * fun_match option
  and typ_bind' = TypBindTypBind of ty_var_seq * id annot * typ * typ_bind option
  and dat_bind' = DatBindDatBind of ty_var_seq * id annot * con_bind * dat_bind option
  and con_bind' = ConBindConBind of id annot * typ option * con_bind option
  and exn_bind' = ExnBindGen of id annot * typ option * exn_bind option
    | ExnBindRepl of id annot * long_id * exn_bind option
  and str' = StrId of long_id
    | StrStruct of dec_list
    | StrTransparent of str * sig_exp
    | StrOpaque of str * sig_exp
    | StrFctApp of id annot * str
    | StrFctAppDec of id annot * dec_list
    | StrLet of dec_list * str
  and str_bind' = StrBindStrBind of id annot * sig_annot option * str * str_bind option
  and sig_annot' = SigAnnotTransparent of sig_exp
    | SigAnnotOpaque of sig_exp
  and sig_exp' = SigExpId of id annot
    | SigExpSig of spec_list
    | SigExpWhere of sig_exp * typ_refin
  and typ_refin' = TypRefinTypRefin of ty_var_seq * long_id * typ * typ_refin option
  and spec' = SpecVal of val_desc
    | SpecType of typ_desc
    | SpecEqtype of typ_desc
    | SpecTypeAbbrev of typ_bind
    | SpecDatatype of dat_desc
    | SpecDatatypeRepl of id annot * long_id
    | SpecException of exn_desc
    | SpecStructure of str_desc
    | SpecSemicolon
    | SpecInclude of sig_exp
    | SpecIncludeMulti of id annot * id annot list
    | SpecSharingType of spec * long_id * long_id list
    | SpecSharing of spec * long_id * long_id list
  and spec_list' = SpecListSpecList of spec list
  and val_desc' = ValDescValDesc of id annot * typ * val_desc option
  and typ_desc' = TypDescTypDesc of ty_var_seq * id annot * typ_desc option
  and dat_desc' = DatDescDatDesc of ty_var_seq * id annot * con_desc * dat_desc option
  and con_desc' = ConDescConDesc of id annot * typ option * con_desc option
  and exn_desc' = ExnDescExnDesc of id annot * typ option * exn_desc option
  and str_desc' = StrDescStrDesc of id annot * sig_exp * str_desc option
  and prog' = ProgDec of dec
    | ProgFunctor of fct_bind
    | ProgSignature of sig_bind
    | ProgSemicolon
  and prog_list' = ProgListProgList of prog list
  and fct_bind' = FctBindPlain of id annot * id annot * sig_exp * sig_annot option * str * fct_bind option
    | FctBindOpened of id annot * spec * sig_annot option * str * fct_bind option
  and sig_bind' = SigBindSigBind of id annot * sig_exp * sig_bind option
  withtype con = con' annot
  and lab = lab' annot
  and long_id = long_id' annot
  and atom_exp = atom_exp' annot
  and exp = exp' annot
  and exp_list_inner = exp_list_inner' annot
  and exp_row = exp_row' annot
  and match = match' annot
  and match_arm = match_arm' annot
  and pat = pat' annot
  and pat_list_inner = pat_list_inner' annot
  and pat_row = pat_row' annot
  and atom_typ = atom_typ' annot
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
    structure Trivial = Trivial
    structure Terminal = struct
      type t = terminal_token
      val lex =
        [ (fn ts =>
            case Terminals.Char.lex ts of
              SOME (v , ts') => SOME (TerminalChar v , ts')
            | NONE => NONE)
        , (fn ts =>
            case Terminals.Float.lex ts of
              SOME (v , ts') => SOME (TerminalFloat v , ts')
            | NONE => NONE)
        , (fn ts =>
            case Terminals.Id.lex ts of
              SOME (v , ts') => SOME (TerminalId v , ts')
            | NONE => NONE)
        , (fn ts =>
            case Terminals.Int.lex ts of
              SOME (v , ts') => SOME (TerminalInt v , ts')
            | NONE => NONE)
        , (fn ts =>
            case Terminals.String.lex ts of
              SOME (v , ts') => SOME (TerminalString v , ts')
            | NONE => NONE)
        , (fn ts =>
            case Terminals.Tyvar.lex ts of
              SOME (v , ts') => SOME (TerminalTyvar v , ts')
            | NONE => NONE)
        , (fn ts =>
            case Terminals.Word.lex ts of
              SOME (v , ts') => SOME (TerminalWord v , ts')
            | NONE => NONE)
        ]
    end
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
    val parseAtomExpDummy : atom_exp t_dummy = dummy ()
    val parseExpDummy : exp t_dummy = dummy ()
    val parseExpListInnerDummy : exp_list_inner t_dummy = dummy ()
    val parseExpRowDummy : exp_row t_dummy = dummy ()
    val parseMatchDummy : match t_dummy = dummy ()
    val parseMatchArmDummy : match_arm t_dummy = dummy ()
    val parsePatDummy : pat t_dummy = dummy ()
    val parsePatListInnerDummy : pat_list_inner t_dummy = dummy ()
    val parsePatRowDummy : pat_row t_dummy = dummy ()
    val parseAtomTypDummy : atom_typ t_dummy = dummy ()
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
  
    (* Con *)
    val parseCon =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseInt =
            create ConInt (
              bind (parseTerminalInt) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseWord =
            create ConWord (
              bind (parseTerminalWord) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseFloat =
            create ConFloat (
              bind (parseTerminalFloat) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseChar =
            create ConChar (
              bind (parseTerminalChar) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseString =
            create ConString (
              bind (parseTerminalString) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseInt
        , parseWord
        , parseFloat
        , parseChar
        , parseString
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
    (* Lab *)
    val parseLab =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseId =
            create LabId (
              bind (parseTerminalId) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseNum =
            create LabNum (
              bind (parseTerminalInt) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseId
        , parseNum
        ]
        end)
  
      in
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
    (* AtomExp *)
    val parseAtomExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseConst =
            create AtomExpConst (
              bind (parseNonterminal (deref parseConDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseOpId =
            create AtomExpOpId (
              bind (keyword 40) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseId =
            create AtomExpId (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseParens =
            create AtomExpParens (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 2) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseTuple =
            create AtomExpTuple (
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
  
          val parseRecord =
            create AtomExpRecord (
              bind (keyword 57) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseExpRowDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 59) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseSelector =
            create AtomExpSelector (
              bind (keyword 0) (fn v0 =>
              bind (parseNonterminal (deref parseLabDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseList =
            create AtomExpList (
              bind (keyword 13) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseExpListInnerDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 14) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseSeq =
            create AtomExpSeq (
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
  
          val parseLet =
            create AtomExpLet (
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
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
    (* Exp *)
    val parseExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseApp =
            create ExpApp (
              bind (plusLongest (
                bind (parseNonterminal (deref parseAtomExpDummy)) (fn v1 =>
                return_node (#node v1) [ annot_add v1 ])))
              (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseCase =
            create ExpCase (
              bind (keyword 20) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 39) (fn v2 =>
              bind (parseNonterminal (deref parseMatchDummy)) (fn v3 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseFn =
            create ExpFn (
              bind (keyword 27) (fn v0 =>
              bind (parseNonterminal (deref parseMatchDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseApp
        , parseCase
        , parseFn
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
        [ (forget parseAtom)
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
          val parseRaise =
            create ExpRaise (
              bind (keyword 43) (fn v0 =>
              bind (parseNonterminal parseLevel5) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseAndAlso =
            create ExpAndAlso (
              bind (parseNonterminal parseLevel5) (fn v0 =>
              bind (keyword 18) (fn v1 =>
              bind (parseNonterminal (forget parseLevel6)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseIf =
            create ExpIf (
              bind (keyword 31) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 50) (fn v2 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v3 =>
              bind (keyword 23) (fn v4 =>
              bind (parseNonterminal parseLevel5) (fn v5 =>
              return_node ((#node v1) , (#node v3) , (#node v5)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 ])))))))
  
          val parseWhile =
            create ExpWhile (
              bind (keyword 54) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 22) (fn v2 =>
              bind (parseNonterminal parseLevel5) (fn v3 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
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
        longest (forget parseLevel4)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
    (* Pat *)
    val parsePat =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseConst =
            create PatConst (
              bind (parseNonterminal (deref parseConDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseWildcard =
            create (fn () => PatWildcard) (
              bind (keyword 15) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseOpVar =
            create PatOpVar (
              bind (keyword 40) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseVar =
            create PatVar (
              bind (parseTerminalId) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseOpCon =
            create PatOpCon (
              bind (keyword 40) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parsePatDummy)) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseParens =
            create PatParens (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parsePatDummy)) (fn v1 =>
              bind (keyword 2) (fn v2 =>
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
  
          val parseRecord =
            create PatRecord (
              bind (keyword 57) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parsePatRowDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 59) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseList =
            create PatList (
              bind (keyword 13) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parsePatListInnerDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 14) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
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
            create PatCon (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              bind (parseNonterminal parseLevel5) (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
          val parseAnnot =
            create PatAnnot (
              bind (parseNonterminal parseLevel5) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
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
  
        in either
        [ (forget parseAtom)
        , parseCon
        , parseAnnot
        , parseOpLayered
        , parseLayered
        ]
        end)
  
      in
        longest (forget parseLevel5)
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
        longest (forget parseAtom)
      end
  
    (* PatRow *)
    val parsePatRow =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseWildcard =
            create (fn () => PatRowWildcard) (
              bind (keyword 7) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
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
  
        in either
        [ parseWildcard
        , parsePat
        , parseVar
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
    (* AtomTyp *)
    val parseAtomTyp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseVar =
            create AtomTypVar (
              bind (parseTerminalTyvar) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseConAppMulti =
            create AtomTypConAppMulti (
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
  
          val parseCon =
            create AtomTypCon (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseParens =
            create AtomTypParens (
              bind (keyword 1) (fn v0 =>
              bind (parseNonterminal (deref parseTypDummy)) (fn v1 =>
              bind (keyword 2) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseRecord =
            create AtomTypRecord (
              bind (keyword 57) (fn v0 =>
              bind (optionalLongest (
                bind (parseNonterminal (deref parseTypRowDummy)) (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 59) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseVar
        , parseConAppMulti
        , parseCon
        , parseParens
        , parseRecord
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseConApp =
            create AtomTypConApp (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ (forget parseAtom)
        , parseConApp
        ]
        end)
  
      in
        longest (forget parseLevel1)
      end
  
    (* Typ *)
    val parseTyp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseInner =
            create TypInner (
              bind (parseNonterminal (deref parseAtomTypDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseTupleTyp =
            create TypTupleTyp (
              bind (parseNonterminal (deref parseAtomTypDummy)) (fn v0 =>
              bind (plusLongest (
                bind (
                  bind (keyword 3) (fn v3 =>
                  bind (parseNonterminal (deref parseAtomTypDummy)) (fn v4 =>
                  return_node (#node v4) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              return_node ((#node v0) , (#node v1)) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseInner
        , parseTupleTyp
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseArrow =
            create TypArrow (
              bind (parseNonterminal (forget parseAtom)) (fn v0 =>
              bind (keyword 5) (fn v1 =>
              bind (parseNonterminal parseLevel1) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseAtom)
        , parseArrow
        ]
        end)
  
      in
        longest (forget parseLevel1)
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
        longest (forget parseAtom)
      end
  
    (* Dec *)
    val parseDec =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseVal =
            create DecVal (
              bind (keyword 52) (fn v0 =>
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v1 =>
              bind (parseNonterminal (deref parseValBindDummy)) (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseFun =
            create DecFun (
              bind (keyword 28) (fn v0 =>
              bind (parseNonterminal (deref parseTyVarSeqDummy)) (fn v1 =>
              bind (parseNonterminal (deref parseFunBindDummy)) (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseType =
            create DecType (
              bind (keyword 51) (fn v0 =>
              bind (parseNonterminal (deref parseTypBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
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
  
          val parseDatatypeRepl =
            create DecDatatypeRepl (
              bind (keyword 21) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (keyword 21) (fn v3 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v4 =>
              return_node ((#node v1) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
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
  
          val parseException =
            create DecException (
              bind (keyword 26) (fn v0 =>
              bind (parseNonterminal (deref parseExnBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseStructure =
            create DecStructure (
              bind (keyword 49) (fn v0 =>
              bind (parseNonterminal (deref parseStrBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseSemicolon =
            create (fn () => DecSemicolon) (
              bind (keyword 10) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseLocal =
            create DecLocal (
              bind (keyword 37) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 32) (fn v2 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v3 =>
              bind (keyword 24) (fn v4 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseOpen =
            create DecOpen (
              bind (keyword 41) (fn v0 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v1 =>
              bind (starLongest (
                bind (parseNonterminal (deref parseLongIdDummy)) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseNonfix =
            create DecNonfix (
              bind (keyword 38) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (starLongest (
                bind (parseTerminalId) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
    (* TyVarSeq *)
    val parseTyVarSeq =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseOne =
            create TyVarSeqOne (
              bind (parseTerminalTyvar) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
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
  
          val parseEmpty =
            create (fn () => TyVarSeqEmpty) (
              empty ())
  
        in either
        [ parseOne
        , parseMany
        , parseEmpty
        ]
        end)
  
      in
        longest (forget parseAtom)
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
        longest (forget parseLevel5)
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
        longest (forget parseAtom)
      end
  
    (* FunMatch *)
    val parseFunMatch =
      let
        val parseAtom = fix (fn parseAtom =>
        let
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
              return_node ((#node v1) , (#node v2) , (#node v3) , (#node v4) , (#node v6) , (#node v7)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 , annot_add v7 ])))))))))
  
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
  
        in either
        [ parseNonfix
        , parseInfix
        , parseInfixParen
        ]
        end)
  
      in
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
    (* ExnBind *)
    val parseExnBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
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
  
        in either
        [ parseGen
        , parseRepl
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
    (* Str *)
    val parseStr =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseId =
            create StrId (
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseStruct =
            create StrStruct (
              bind (keyword 48) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 24) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseFctApp =
            create StrFctApp (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v2 =>
              bind (keyword 2) (fn v3 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseFctAppDec =
            create StrFctAppDec (
              bind (parseTerminalId) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v2 =>
              bind (keyword 2) (fn v3 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 ])))))
  
          val parseLet =
            create StrLet (
              bind (keyword 36) (fn v0 =>
              bind (parseNonterminal (deref parseDecListDummy)) (fn v1 =>
              bind (keyword 32) (fn v2 =>
              bind (parseNonterminal (deref parseStrDummy)) (fn v3 =>
              bind (keyword 24) (fn v4 =>
              return_node ((#node v1) , (#node v3)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
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
            create StrTransparent (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 8) (fn v1 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseOpaque =
            create StrOpaque (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 9) (fn v1 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseAtom)
        , parseTransparent
        , parseOpaque
        ]
        end)
  
      in
        longest (forget parseLevel1)
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
        longest (forget parseAtom)
      end
  
    (* SigAnnot *)
    val parseSigAnnot =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseTransparent =
            create SigAnnotTransparent (
              bind (keyword 8) (fn v0 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseOpaque =
            create SigAnnotOpaque (
              bind (keyword 9) (fn v0 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
        in either
        [ parseTransparent
        , parseOpaque
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
    (* SigExp *)
    val parseSigExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseId =
            create SigExpId (
              bind (parseTerminalId) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseSig =
            create SigExpSig (
              bind (keyword 46) (fn v0 =>
              bind (parseNonterminal (deref parseSpecListDummy)) (fn v1 =>
              bind (keyword 24) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseId
        , parseSig
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
        longest (forget parseLevel1)
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
        longest (forget parseAtom)
      end
  
    (* Spec *)
    val parseSpec =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseVal =
            create SpecVal (
              bind (keyword 52) (fn v0 =>
              bind (parseNonterminal (deref parseValDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseType =
            create SpecType (
              bind (keyword 51) (fn v0 =>
              bind (parseNonterminal (deref parseTypDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseEqtype =
            create SpecEqtype (
              bind (keyword 25) (fn v0 =>
              bind (parseNonterminal (deref parseTypDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseTypeAbbrev =
            create SpecTypeAbbrev (
              bind (keyword 51) (fn v0 =>
              bind (parseNonterminal (deref parseTypBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseDatatype =
            create SpecDatatype (
              bind (keyword 21) (fn v0 =>
              bind (parseNonterminal (deref parseDatDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseDatatypeRepl =
            create SpecDatatypeRepl (
              bind (keyword 21) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (keyword 11) (fn v2 =>
              bind (keyword 21) (fn v3 =>
              bind (parseNonterminal (deref parseLongIdDummy)) (fn v4 =>
              return_node ((#node v1) , (#node v4)) [ annot_add v0 , annot_add v1 , annot_add v2 , annot_add v3 , annot_add v4 ]))))))
  
          val parseException =
            create SpecException (
              bind (keyword 26) (fn v0 =>
              bind (parseNonterminal (deref parseExnDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseStructure =
            create SpecStructure (
              bind (keyword 49) (fn v0 =>
              bind (parseNonterminal (deref parseStrDescDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseSemicolon =
            create (fn () => SpecSemicolon) (
              bind (keyword 10) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseInclude =
            create SpecInclude (
              bind (keyword 33) (fn v0 =>
              bind (parseNonterminal (deref parseSigExpDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseIncludeMulti =
            create SpecIncludeMulti (
              bind (keyword 33) (fn v0 =>
              bind (parseTerminalId) (fn v1 =>
              bind (starLongest (
                bind (parseTerminalId) (fn v3 =>
                return_node (#node v3) [ annot_add v3 ])))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
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
  
        in either
        [ (forget parseAtom)
        , parseSharingType
        , parseSharing
        ]
        end)
  
      in
        longest (forget parseLevel1)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
    (* Prog *)
    val parseProg =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseDec =
            create ProgDec (
              bind (parseNonterminal (deref parseDecDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseFunctor =
            create ProgFunctor (
              bind (keyword 29) (fn v0 =>
              bind (parseNonterminal (deref parseFctBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseSignature =
            create ProgSignature (
              bind (keyword 47) (fn v0 =>
              bind (parseNonterminal (deref parseSigBindDummy)) (fn v1 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 ])))
  
          val parseSemicolon =
            create (fn () => ProgSemicolon) (
              bind (keyword 10) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
        in either
        [ parseDec
        , parseFunctor
        , parseSignature
        , parseSemicolon
        ]
        end)
  
      in
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
    (* FctBind *)
    val parseFctBind =
      let
        val parseAtom = fix (fn parseAtom =>
        let
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
  
        in either
        [ parsePlain
        , parseOpened
        ]
        end)
  
      in
        longest (forget parseAtom)
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
        longest (forget parseAtom)
      end
  
  
  val () = set parseConDummy parseCon
  val () = set parseLabDummy parseLab
  val () = set parseLongIdDummy parseLongId
  val () = set parseAtomExpDummy parseAtomExp
  val () = set parseExpDummy parseExp
  val () = set parseExpListInnerDummy parseExpListInner
  val () = set parseExpRowDummy parseExpRow
  val () = set parseMatchDummy parseMatch
  val () = set parseMatchArmDummy parseMatchArm
  val () = set parsePatDummy parsePat
  val () = set parsePatListInnerDummy parsePatListInner
  val () = set parsePatRowDummy parsePatRow
  val () = set parseAtomTypDummy parseAtomTyp
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

functor SmlPrint (
  structure Ast : SML_AST
  structure Terminals : sig
    structure Char : PRINT_TERMINAL where type t = Ast.char
    structure Float : PRINT_TERMINAL where type t = Ast.float
    structure Id : PRINT_TERMINAL where type t = Ast.id
    structure Int : PRINT_TERMINAL where type t = Ast.int
    structure String : PRINT_TERMINAL where type t = Ast.string
    structure Tyvar : PRINT_TERMINAL where type t = Ast.tyvar
    structure Word : PRINT_TERMINAL where type t = Ast.word
  end
) :>
sig
  val printChar : Ast.char Ast.annot -> string
  val printFloat : Ast.float Ast.annot -> string
  val printId : Ast.id Ast.annot -> string
  val printInt : Ast.int Ast.annot -> string
  val printString : Ast.string Ast.annot -> string
  val printTyvar : Ast.tyvar Ast.annot -> string
  val printWord : Ast.word Ast.annot -> string
  val printCon : Ast.con -> string
  val printLab : Ast.lab -> string
  val printLongId : Ast.long_id -> string
  val printAtomExp : Ast.atom_exp -> string
  val printExp : Ast.exp -> string
  val printExpListInner : Ast.exp_list_inner -> string
  val printExpRow : Ast.exp_row -> string
  val printMatch : Ast.match -> string
  val printMatchArm : Ast.match_arm -> string
  val printPat : Ast.pat -> string
  val printPatListInner : Ast.pat_list_inner -> string
  val printPatRow : Ast.pat_row -> string
  val printAtomTyp : Ast.atom_typ -> string
  val printTyp : Ast.typ -> string
  val printTypRow : Ast.typ_row -> string
  val printDec : Ast.dec -> string
  val printDecList : Ast.dec_list -> string
  val printTyVarSeq : Ast.ty_var_seq -> string
  val printValBind : Ast.val_bind -> string
  val printFunBind : Ast.fun_bind -> string
  val printFunMatch : Ast.fun_match -> string
  val printTypBind : Ast.typ_bind -> string
  val printDatBind : Ast.dat_bind -> string
  val printConBind : Ast.con_bind -> string
  val printExnBind : Ast.exn_bind -> string
  val printStr : Ast.str -> string
  val printStrBind : Ast.str_bind -> string
  val printSigAnnot : Ast.sig_annot -> string
  val printSigExp : Ast.sig_exp -> string
  val printTypRefin : Ast.typ_refin -> string
  val printSpec : Ast.spec -> string
  val printSpecList : Ast.spec_list -> string
  val printValDesc : Ast.val_desc -> string
  val printTypDesc : Ast.typ_desc -> string
  val printDatDesc : Ast.dat_desc -> string
  val printConDesc : Ast.con_desc -> string
  val printExnDesc : Ast.exn_desc -> string
  val printStrDesc : Ast.str_desc -> string
  val printProg : Ast.prog -> string
  val printProgList : Ast.prog_list -> string
  val printFctBind : Ast.fct_bind -> string
  val printSigBind : Ast.sig_bind -> string
  val prettyPrintChar : Ast.char Ast.annot -> string
  val prettyPrintFloat : Ast.float Ast.annot -> string
  val prettyPrintId : Ast.id Ast.annot -> string
  val prettyPrintInt : Ast.int Ast.annot -> string
  val prettyPrintString : Ast.string Ast.annot -> string
  val prettyPrintTyvar : Ast.tyvar Ast.annot -> string
  val prettyPrintWord : Ast.word Ast.annot -> string
  val prettyPrintCon : Ast.con -> string
  val prettyPrintLab : Ast.lab -> string
  val prettyPrintLongId : Ast.long_id -> string
  val prettyPrintAtomExp : Ast.atom_exp -> string
  val prettyPrintExp : Ast.exp -> string
  val prettyPrintExpListInner : Ast.exp_list_inner -> string
  val prettyPrintExpRow : Ast.exp_row -> string
  val prettyPrintMatch : Ast.match -> string
  val prettyPrintMatchArm : Ast.match_arm -> string
  val prettyPrintPat : Ast.pat -> string
  val prettyPrintPatListInner : Ast.pat_list_inner -> string
  val prettyPrintPatRow : Ast.pat_row -> string
  val prettyPrintAtomTyp : Ast.atom_typ -> string
  val prettyPrintTyp : Ast.typ -> string
  val prettyPrintTypRow : Ast.typ_row -> string
  val prettyPrintDec : Ast.dec -> string
  val prettyPrintDecList : Ast.dec_list -> string
  val prettyPrintTyVarSeq : Ast.ty_var_seq -> string
  val prettyPrintValBind : Ast.val_bind -> string
  val prettyPrintFunBind : Ast.fun_bind -> string
  val prettyPrintFunMatch : Ast.fun_match -> string
  val prettyPrintTypBind : Ast.typ_bind -> string
  val prettyPrintDatBind : Ast.dat_bind -> string
  val prettyPrintConBind : Ast.con_bind -> string
  val prettyPrintExnBind : Ast.exn_bind -> string
  val prettyPrintStr : Ast.str -> string
  val prettyPrintStrBind : Ast.str_bind -> string
  val prettyPrintSigAnnot : Ast.sig_annot -> string
  val prettyPrintSigExp : Ast.sig_exp -> string
  val prettyPrintTypRefin : Ast.typ_refin -> string
  val prettyPrintSpec : Ast.spec -> string
  val prettyPrintSpecList : Ast.spec_list -> string
  val prettyPrintValDesc : Ast.val_desc -> string
  val prettyPrintTypDesc : Ast.typ_desc -> string
  val prettyPrintDatDesc : Ast.dat_desc -> string
  val prettyPrintConDesc : Ast.con_desc -> string
  val prettyPrintExnDesc : Ast.exn_desc -> string
  val prettyPrintStrDesc : Ast.str_desc -> string
  val prettyPrintProg : Ast.prog -> string
  val prettyPrintProgList : Ast.prog_list -> string
  val prettyPrintFctBind : Ast.fct_bind -> string
  val prettyPrintSigBind : Ast.sig_bind -> string
  
end = struct

  open Ast
  
  val push = PrintBuffer.push
  
  fun printChar buf
    ( { node , span = { start = { lineno , ... } , ... } } : char annot ) =
    push buf (Terminals.Char.show node) lineno
  
  fun printFloat buf
    ( { node , span = { start = { lineno , ... } , ... } } : float annot ) =
    push buf (Terminals.Float.show node) lineno
  
  fun printId buf
    ( { node , span = { start = { lineno , ... } , ... } } : id annot ) =
    push buf (Terminals.Id.show node) lineno
  
  fun printInt buf
    ( { node , span = { start = { lineno , ... } , ... } } : int annot ) =
    push buf (Terminals.Int.show node) lineno
  
  fun printString buf
    ( { node , span = { start = { lineno , ... } , ... } } : string annot ) =
    push buf (Terminals.String.show node) lineno
  
  fun printTyvar buf
    ( { node , span = { start = { lineno , ... } , ... } } : tyvar annot ) =
    push buf (Terminals.Tyvar.show node) lineno
  
  fun printWord buf
    ( { node , span = { start = { lineno , ... } , ... } } : word annot ) =
    push buf (Terminals.Word.show node) lineno
  
  fun printCon buf
    ( { node , span = { start = { lineno , ... } , ... } } : con ) =
    case node of
        ConInt v0 =>
          ( push buf "ConInt" lineno
          ; push buf "(" lineno
          ; printInt buf v0
          ; push buf ")" lineno
          )
      | ConWord v0 =>
          ( push buf "ConWord" lineno
          ; push buf "(" lineno
          ; printWord buf v0
          ; push buf ")" lineno
          )
      | ConFloat v0 =>
          ( push buf "ConFloat" lineno
          ; push buf "(" lineno
          ; printFloat buf v0
          ; push buf ")" lineno
          )
      | ConChar v0 =>
          ( push buf "ConChar" lineno
          ; push buf "(" lineno
          ; printChar buf v0
          ; push buf ")" lineno
          )
      | ConString v0 =>
          ( push buf "ConString" lineno
          ; push buf "(" lineno
          ; printString buf v0
          ; push buf ")" lineno
          )
  
  and printLab buf
    ( { node , span = { start = { lineno , ... } , ... } } : lab ) =
    case node of
        LabId v0 =>
          ( push buf "LabId" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf ")" lineno
          )
      | LabNum v0 =>
          ( push buf "LabNum" lineno
          ; push buf "(" lineno
          ; printInt buf v0
          ; push buf ")" lineno
          )
  
  and printLongId buf
    ( { node , span = { start = { lineno , ... } , ... } } : long_id ) =
    case node of
        LongIdLongId (v0 , v1) =>
          ( push buf "LongIdLongId" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printId buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printAtomExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : atom_exp ) =
    case node of
        AtomExpConst v0 =>
          ( push buf "AtomExpConst" lineno
          ; push buf "(" lineno
          ; printCon buf v0
          ; push buf ")" lineno
          )
      | AtomExpOpId v0 =>
          ( push buf "AtomExpOpId" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf ")" lineno
          )
      | AtomExpId v0 =>
          ( push buf "AtomExpId" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf ")" lineno
          )
      | AtomExpParens v0 =>
          ( push buf "AtomExpParens" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf ")" lineno
          )
      | AtomExpTuple (v0 , v1 , v2) =>
          ( push buf "AtomExpTuple" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printExp buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | AtomExpRecord v0 =>
          ( push buf "AtomExpRecord" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printExpRow buf v0v)
          ; push buf ")" lineno
          )
      | AtomExpSelector v0 =>
          ( push buf "AtomExpSelector" lineno
          ; push buf "(" lineno
          ; printLab buf v0
          ; push buf ")" lineno
          )
      | AtomExpList v0 =>
          ( push buf "AtomExpList" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printExpListInner buf v0v)
          ; push buf ")" lineno
          )
      | AtomExpSeq (v0 , v1) =>
          ( push buf "AtomExpSeq" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printExp buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | AtomExpLet (v0 , v1 , v2) =>
          ( push buf "AtomExpLet" lineno
          ; push buf "(" lineno
          ; printDecList buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printExp buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp ) =
    case node of
        ExpApp v0 =>
          ( push buf "ExpApp" lineno
          ; push buf "(" lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v0e ) => ( if i > 0 then push buf " , " lineno else () ; printAtomExp buf v0e )) v0 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | ExpAnnot (v0 , v1) =>
          ( push buf "ExpAnnot" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printTyp buf v1
          ; push buf ")" lineno
          )
      | ExpRaise v0 =>
          ( push buf "ExpRaise" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf ")" lineno
          )
      | ExpHandle (v0 , v1) =>
          ( push buf "ExpHandle" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printMatch buf v1
          ; push buf ")" lineno
          )
      | ExpAndAlso (v0 , v1) =>
          ( push buf "ExpAndAlso" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpOrElse (v0 , v1) =>
          ( push buf "ExpOrElse" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpIf (v0 , v1 , v2) =>
          ( push buf "ExpIf" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf " , " lineno
          ; printExp buf v2
          ; push buf ")" lineno
          )
      | ExpWhile (v0 , v1) =>
          ( push buf "ExpWhile" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpCase (v0 , v1) =>
          ( push buf "ExpCase" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printMatch buf v1
          ; push buf ")" lineno
          )
      | ExpFn v0 =>
          ( push buf "ExpFn" lineno
          ; push buf "(" lineno
          ; printMatch buf v0
          ; push buf ")" lineno
          )
  
  and printExpListInner buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp_list_inner ) =
    case node of
        ExpListInnerExpListInner (v0 , v1) =>
          ( push buf "ExpListInnerExpListInner" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printExp buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printExpRow buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp_row ) =
    case node of
        ExpRowExpRow (v0 , v1 , v2) =>
          ( push buf "ExpRowExpRow" lineno
          ; push buf "(" lineno
          ; printLab buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; let val (v2e0 , v2e1) = v2e in push buf "(" lineno ; printLab buf v2e0 ; push buf " , " lineno ; printExp buf v2e1 ; push buf ")" lineno end )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printMatch buf
    ( { node , span = { start = { lineno , ... } , ... } } : match ) =
    case node of
        MatchMatch (v0 , v1) =>
          ( push buf "MatchMatch" lineno
          ; push buf "(" lineno
          ; printMatchArm buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printMatchArm buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printMatchArm buf
    ( { node , span = { start = { lineno , ... } , ... } } : match_arm ) =
    case node of
        MatchArmMatchArm (v0 , v1) =>
          ( push buf "MatchArmMatchArm" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
  
  and printPat buf
    ( { node , span = { start = { lineno , ... } , ... } } : pat ) =
    case node of
        PatConst v0 =>
          ( push buf "PatConst" lineno
          ; push buf "(" lineno
          ; printCon buf v0
          ; push buf ")" lineno
          )
      | PatWildcard =>
          ( push buf "PatWildcard" lineno
          )
      | PatOpVar v0 =>
          ( push buf "PatOpVar" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf ")" lineno
          )
      | PatVar v0 =>
          ( push buf "PatVar" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf ")" lineno
          )
      | PatOpCon (v0 , v1) =>
          ( push buf "PatOpCon" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printPat buf v1v)
          ; push buf ")" lineno
          )
      | PatCon (v0 , v1) =>
          ( push buf "PatCon" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf " , " lineno
          ; printPat buf v1
          ; push buf ")" lineno
          )
      | PatParens v0 =>
          ( push buf "PatParens" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf ")" lineno
          )
      | PatTuple (v0 , v1 , v2) =>
          ( push buf "PatTuple" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; printPat buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printPat buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | PatRecord v0 =>
          ( push buf "PatRecord" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printPatRow buf v0v)
          ; push buf ")" lineno
          )
      | PatList v0 =>
          ( push buf "PatList" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printPatListInner buf v0v)
          ; push buf ")" lineno
          )
      | PatAnnot (v0 , v1) =>
          ( push buf "PatAnnot" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; printTyp buf v1
          ; push buf ")" lineno
          )
      | PatOpLayered (v0 , v1 , v2) =>
          ( push buf "PatOpLayered" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; printPat buf v2
          ; push buf ")" lineno
          )
      | PatLayered (v0 , v1 , v2) =>
          ( push buf "PatLayered" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; printPat buf v2
          ; push buf ")" lineno
          )
  
  and printPatListInner buf
    ( { node , span = { start = { lineno , ... } , ... } } : pat_list_inner ) =
    case node of
        PatListInnerPatListInner (v0 , v1) =>
          ( push buf "PatListInnerPatListInner" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printPat buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printPatRow buf
    ( { node , span = { start = { lineno , ... } , ... } } : pat_row ) =
    case node of
        PatRowWildcard =>
          ( push buf "PatRowWildcard" lineno
          )
      | PatRowPat (v0 , v1 , v2) =>
          ( push buf "PatRowPat" lineno
          ; push buf "(" lineno
          ; printLab buf v0
          ; push buf " , " lineno
          ; printPat buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printPatRow buf v2v)
          ; push buf ")" lineno
          )
      | PatRowVar (v0 , v1 , v2 , v3) =>
          ( push buf "PatRowVar" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printPat buf v2v)
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printPatRow buf v3v)
          ; push buf ")" lineno
          )
  
  and printAtomTyp buf
    ( { node , span = { start = { lineno , ... } , ... } } : atom_typ ) =
    case node of
        AtomTypVar v0 =>
          ( push buf "AtomTypVar" lineno
          ; push buf "(" lineno
          ; printTyvar buf v0
          ; push buf ")" lineno
          )
      | AtomTypConApp (v0 , v1) =>
          ( push buf "AtomTypConApp" lineno
          ; push buf "(" lineno
          ; printAtomTyp buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf ")" lineno
          )
      | AtomTypConAppMulti (v0 , v1 , v2 , v3) =>
          ( push buf "AtomTypConAppMulti" lineno
          ; push buf "(" lineno
          ; printTyp buf v0
          ; push buf " , " lineno
          ; printTyp buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printTyp buf v2e )) v2 ; push buf "]" lineno )
          ; push buf " , " lineno
          ; printLongId buf v3
          ; push buf ")" lineno
          )
      | AtomTypCon v0 =>
          ( push buf "AtomTypCon" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf ")" lineno
          )
      | AtomTypParens v0 =>
          ( push buf "AtomTypParens" lineno
          ; push buf "(" lineno
          ; printTyp buf v0
          ; push buf ")" lineno
          )
      | AtomTypRecord v0 =>
          ( push buf "AtomTypRecord" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printTypRow buf v0v)
          ; push buf ")" lineno
          )
  
  and printTyp buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ ) =
    case node of
        TypInner v0 =>
          ( push buf "TypInner" lineno
          ; push buf "(" lineno
          ; printAtomTyp buf v0
          ; push buf ")" lineno
          )
      | TypTupleTyp (v0 , v1) =>
          ( push buf "TypTupleTyp" lineno
          ; push buf "(" lineno
          ; printAtomTyp buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printAtomTyp buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | TypArrow (v0 , v1) =>
          ( push buf "TypArrow" lineno
          ; push buf "(" lineno
          ; printTyp buf v0
          ; push buf " , " lineno
          ; printTyp buf v1
          ; push buf ")" lineno
          )
  
  and printTypRow buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_row ) =
    case node of
        TypRowTypRow (v0 , v1 , v2) =>
          ( push buf "TypRowTypRow" lineno
          ; push buf "(" lineno
          ; printLab buf v0
          ; push buf " , " lineno
          ; printTyp buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; let val (v2e0 , v2e1) = v2e in push buf "(" lineno ; printLab buf v2e0 ; push buf " , " lineno ; printTyp buf v2e1 ; push buf ")" lineno end )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printDec buf
    ( { node , span = { start = { lineno , ... } , ... } } : dec ) =
    case node of
        DecVal (v0 , v1) =>
          ( push buf "DecVal" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printValBind buf v1
          ; push buf ")" lineno
          )
      | DecFun (v0 , v1) =>
          ( push buf "DecFun" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printFunBind buf v1
          ; push buf ")" lineno
          )
      | DecType v0 =>
          ( push buf "DecType" lineno
          ; push buf "(" lineno
          ; printTypBind buf v0
          ; push buf ")" lineno
          )
      | DecDatatype (v0 , v1) =>
          ( push buf "DecDatatype" lineno
          ; push buf "(" lineno
          ; printDatBind buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTypBind buf v1v)
          ; push buf ")" lineno
          )
      | DecDatatypeRepl (v0 , v1) =>
          ( push buf "DecDatatypeRepl" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf ")" lineno
          )
      | DecAbstype (v0 , v1 , v2) =>
          ( push buf "DecAbstype" lineno
          ; push buf "(" lineno
          ; printDatBind buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTypBind buf v1v)
          ; push buf " , " lineno
          ; printDecList buf v2
          ; push buf ")" lineno
          )
      | DecException v0 =>
          ( push buf "DecException" lineno
          ; push buf "(" lineno
          ; printExnBind buf v0
          ; push buf ")" lineno
          )
      | DecStructure v0 =>
          ( push buf "DecStructure" lineno
          ; push buf "(" lineno
          ; printStrBind buf v0
          ; push buf ")" lineno
          )
      | DecSemicolon =>
          ( push buf "DecSemicolon" lineno
          )
      | DecLocal (v0 , v1) =>
          ( push buf "DecLocal" lineno
          ; push buf "(" lineno
          ; printDecList buf v0
          ; push buf " , " lineno
          ; printDecList buf v1
          ; push buf ")" lineno
          )
      | DecOpen (v0 , v1) =>
          ( push buf "DecOpen" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printLongId buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | DecNonfix (v0 , v1) =>
          ( push buf "DecNonfix" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printId buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | DecInfix (v0 , v1 , v2) =>
          ( push buf "DecInfix" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printInt buf v0v)
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printId buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | DecInfixr (v0 , v1 , v2) =>
          ( push buf "DecInfixr" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => printInt buf v0v)
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printId buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printDecList buf
    ( { node , span = { start = { lineno , ... } , ... } } : dec_list ) =
    case node of
        DecListDecList v0 =>
          ( push buf "DecListDecList" lineno
          ; push buf "(" lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v0e ) => ( if i > 0 then push buf " , " lineno else () ; printDec buf v0e )) v0 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printTyVarSeq buf
    ( { node , span = { start = { lineno , ... } , ... } } : ty_var_seq ) =
    case node of
        TyVarSeqOne v0 =>
          ( push buf "TyVarSeqOne" lineno
          ; push buf "(" lineno
          ; printTyvar buf v0
          ; push buf ")" lineno
          )
      | TyVarSeqMany (v0 , v1) =>
          ( push buf "TyVarSeqMany" lineno
          ; push buf "(" lineno
          ; printTyvar buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printTyvar buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | TyVarSeqEmpty =>
          ( push buf "TyVarSeqEmpty" lineno
          )
  
  and printValBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : val_bind ) =
    case node of
        ValBindValBind (v0 , v1 , v2) =>
          ( push buf "ValBindValBind" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printValBind buf v2v)
          ; push buf ")" lineno
          )
      | ValBindRec v0 =>
          ( push buf "ValBindRec" lineno
          ; push buf "(" lineno
          ; printValBind buf v0
          ; push buf ")" lineno
          )
  
  and printFunBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : fun_bind ) =
    case node of
        FunBindFunBind (v0 , v1) =>
          ( push buf "FunBindFunBind" lineno
          ; push buf "(" lineno
          ; printFunMatch buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printFunBind buf v1v)
          ; push buf ")" lineno
          )
  
  and printFunMatch buf
    ( { node , span = { start = { lineno , ... } , ... } } : fun_match ) =
    case node of
        FunMatchNonfix (v0 , v1 , v2 , v3 , v4 , v5) =>
          ( push buf "FunMatchNonfix" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printPat buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printPat buf v2e )) v2 ; push buf "]" lineno )
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printTyp buf v3v)
          ; push buf " , " lineno
          ; printExp buf v4
          ; push buf " , " lineno
          ; (case v5 of NONE => push buf "_" lineno | SOME v5v => printFunMatch buf v5v)
          ; push buf ")" lineno
          )
      | FunMatchInfix (v0 , v1 , v2 , v3 , v4 , v5) =>
          ( push buf "FunMatchInfix" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; printPat buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printTyp buf v3v)
          ; push buf " , " lineno
          ; printExp buf v4
          ; push buf " , " lineno
          ; (case v5 of NONE => push buf "_" lineno | SOME v5v => printFunMatch buf v5v)
          ; push buf ")" lineno
          )
      | FunMatchInfixParen (v0 , v1 , v2 , v3 , v4 , v5 , v6) =>
          ( push buf "FunMatchInfixParen" lineno
          ; push buf "(" lineno
          ; printPat buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; printPat buf v2
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v3e ) => ( if i > 0 then push buf " , " lineno else () ; printPat buf v3e )) v3 ; push buf "]" lineno )
          ; push buf " , " lineno
          ; (case v4 of NONE => push buf "_" lineno | SOME v4v => printTyp buf v4v)
          ; push buf " , " lineno
          ; printExp buf v5
          ; push buf " , " lineno
          ; (case v6 of NONE => push buf "_" lineno | SOME v6v => printFunMatch buf v6v)
          ; push buf ")" lineno
          )
  
  and printTypBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_bind ) =
    case node of
        TypBindTypBind (v0 , v1 , v2 , v3) =>
          ( push buf "TypBindTypBind" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; printTyp buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printTypBind buf v3v)
          ; push buf ")" lineno
          )
  
  and printDatBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : dat_bind ) =
    case node of
        DatBindDatBind (v0 , v1 , v2 , v3) =>
          ( push buf "DatBindDatBind" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; printConBind buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printDatBind buf v3v)
          ; push buf ")" lineno
          )
  
  and printConBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : con_bind ) =
    case node of
        ConBindConBind (v0 , v1 , v2) =>
          ( push buf "ConBindConBind" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printConBind buf v2v)
          ; push buf ")" lineno
          )
  
  and printExnBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : exn_bind ) =
    case node of
        ExnBindGen (v0 , v1 , v2) =>
          ( push buf "ExnBindGen" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printExnBind buf v2v)
          ; push buf ")" lineno
          )
      | ExnBindRepl (v0 , v1 , v2) =>
          ( push buf "ExnBindRepl" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printExnBind buf v2v)
          ; push buf ")" lineno
          )
  
  and printStr buf
    ( { node , span = { start = { lineno , ... } , ... } } : str ) =
    case node of
        StrId v0 =>
          ( push buf "StrId" lineno
          ; push buf "(" lineno
          ; printLongId buf v0
          ; push buf ")" lineno
          )
      | StrStruct v0 =>
          ( push buf "StrStruct" lineno
          ; push buf "(" lineno
          ; printDecList buf v0
          ; push buf ")" lineno
          )
      | StrTransparent (v0 , v1) =>
          ( push buf "StrTransparent" lineno
          ; push buf "(" lineno
          ; printStr buf v0
          ; push buf " , " lineno
          ; printSigExp buf v1
          ; push buf ")" lineno
          )
      | StrOpaque (v0 , v1) =>
          ( push buf "StrOpaque" lineno
          ; push buf "(" lineno
          ; printStr buf v0
          ; push buf " , " lineno
          ; printSigExp buf v1
          ; push buf ")" lineno
          )
      | StrFctApp (v0 , v1) =>
          ( push buf "StrFctApp" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printStr buf v1
          ; push buf ")" lineno
          )
      | StrFctAppDec (v0 , v1) =>
          ( push buf "StrFctAppDec" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printDecList buf v1
          ; push buf ")" lineno
          )
      | StrLet (v0 , v1) =>
          ( push buf "StrLet" lineno
          ; push buf "(" lineno
          ; printDecList buf v0
          ; push buf " , " lineno
          ; printStr buf v1
          ; push buf ")" lineno
          )
  
  and printStrBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : str_bind ) =
    case node of
        StrBindStrBind (v0 , v1 , v2 , v3) =>
          ( push buf "StrBindStrBind" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printSigAnnot buf v1v)
          ; push buf " , " lineno
          ; printStr buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printStrBind buf v3v)
          ; push buf ")" lineno
          )
  
  and printSigAnnot buf
    ( { node , span = { start = { lineno , ... } , ... } } : sig_annot ) =
    case node of
        SigAnnotTransparent v0 =>
          ( push buf "SigAnnotTransparent" lineno
          ; push buf "(" lineno
          ; printSigExp buf v0
          ; push buf ")" lineno
          )
      | SigAnnotOpaque v0 =>
          ( push buf "SigAnnotOpaque" lineno
          ; push buf "(" lineno
          ; printSigExp buf v0
          ; push buf ")" lineno
          )
  
  and printSigExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : sig_exp ) =
    case node of
        SigExpId v0 =>
          ( push buf "SigExpId" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf ")" lineno
          )
      | SigExpSig v0 =>
          ( push buf "SigExpSig" lineno
          ; push buf "(" lineno
          ; printSpecList buf v0
          ; push buf ")" lineno
          )
      | SigExpWhere (v0 , v1) =>
          ( push buf "SigExpWhere" lineno
          ; push buf "(" lineno
          ; printSigExp buf v0
          ; push buf " , " lineno
          ; printTypRefin buf v1
          ; push buf ")" lineno
          )
  
  and printTypRefin buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_refin ) =
    case node of
        TypRefinTypRefin (v0 , v1 , v2 , v3) =>
          ( push buf "TypRefinTypRefin" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf " , " lineno
          ; printTyp buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printTypRefin buf v3v)
          ; push buf ")" lineno
          )
  
  and printSpec buf
    ( { node , span = { start = { lineno , ... } , ... } } : spec ) =
    case node of
        SpecVal v0 =>
          ( push buf "SpecVal" lineno
          ; push buf "(" lineno
          ; printValDesc buf v0
          ; push buf ")" lineno
          )
      | SpecType v0 =>
          ( push buf "SpecType" lineno
          ; push buf "(" lineno
          ; printTypDesc buf v0
          ; push buf ")" lineno
          )
      | SpecEqtype v0 =>
          ( push buf "SpecEqtype" lineno
          ; push buf "(" lineno
          ; printTypDesc buf v0
          ; push buf ")" lineno
          )
      | SpecTypeAbbrev v0 =>
          ( push buf "SpecTypeAbbrev" lineno
          ; push buf "(" lineno
          ; printTypBind buf v0
          ; push buf ")" lineno
          )
      | SpecDatatype v0 =>
          ( push buf "SpecDatatype" lineno
          ; push buf "(" lineno
          ; printDatDesc buf v0
          ; push buf ")" lineno
          )
      | SpecDatatypeRepl (v0 , v1) =>
          ( push buf "SpecDatatypeRepl" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf ")" lineno
          )
      | SpecException v0 =>
          ( push buf "SpecException" lineno
          ; push buf "(" lineno
          ; printExnDesc buf v0
          ; push buf ")" lineno
          )
      | SpecStructure v0 =>
          ( push buf "SpecStructure" lineno
          ; push buf "(" lineno
          ; printStrDesc buf v0
          ; push buf ")" lineno
          )
      | SpecSemicolon =>
          ( push buf "SpecSemicolon" lineno
          )
      | SpecInclude v0 =>
          ( push buf "SpecInclude" lineno
          ; push buf "(" lineno
          ; printSigExp buf v0
          ; push buf ")" lineno
          )
      | SpecIncludeMulti (v0 , v1) =>
          ( push buf "SpecIncludeMulti" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v1e ) => ( if i > 0 then push buf " , " lineno else () ; printId buf v1e )) v1 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | SpecSharingType (v0 , v1 , v2) =>
          ( push buf "SpecSharingType" lineno
          ; push buf "(" lineno
          ; printSpec buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printLongId buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
      | SpecSharing (v0 , v1 , v2) =>
          ( push buf "SpecSharing" lineno
          ; push buf "(" lineno
          ; printSpec buf v0
          ; push buf " , " lineno
          ; printLongId buf v1
          ; push buf " , " lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v2e ) => ( if i > 0 then push buf " , " lineno else () ; printLongId buf v2e )) v2 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printSpecList buf
    ( { node , span = { start = { lineno , ... } , ... } } : spec_list ) =
    case node of
        SpecListSpecList v0 =>
          ( push buf "SpecListSpecList" lineno
          ; push buf "(" lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v0e ) => ( if i > 0 then push buf " , " lineno else () ; printSpec buf v0e )) v0 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printValDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : val_desc ) =
    case node of
        ValDescValDesc (v0 , v1 , v2) =>
          ( push buf "ValDescValDesc" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printTyp buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printValDesc buf v2v)
          ; push buf ")" lineno
          )
  
  and printTypDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_desc ) =
    case node of
        TypDescTypDesc (v0 , v1 , v2) =>
          ( push buf "TypDescTypDesc" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printTypDesc buf v2v)
          ; push buf ")" lineno
          )
  
  and printDatDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : dat_desc ) =
    case node of
        DatDescDatDesc (v0 , v1 , v2 , v3) =>
          ( push buf "DatDescDatDesc" lineno
          ; push buf "(" lineno
          ; printTyVarSeq buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; printConDesc buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printDatDesc buf v3v)
          ; push buf ")" lineno
          )
  
  and printConDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : con_desc ) =
    case node of
        ConDescConDesc (v0 , v1 , v2) =>
          ( push buf "ConDescConDesc" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printConDesc buf v2v)
          ; push buf ")" lineno
          )
  
  and printExnDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : exn_desc ) =
    case node of
        ExnDescExnDesc (v0 , v1 , v2) =>
          ( push buf "ExnDescExnDesc" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; (case v1 of NONE => push buf "_" lineno | SOME v1v => printTyp buf v1v)
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printExnDesc buf v2v)
          ; push buf ")" lineno
          )
  
  and printStrDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : str_desc ) =
    case node of
        StrDescStrDesc (v0 , v1 , v2) =>
          ( push buf "StrDescStrDesc" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printSigExp buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printStrDesc buf v2v)
          ; push buf ")" lineno
          )
  
  and printProg buf
    ( { node , span = { start = { lineno , ... } , ... } } : prog ) =
    case node of
        ProgDec v0 =>
          ( push buf "ProgDec" lineno
          ; push buf "(" lineno
          ; printDec buf v0
          ; push buf ")" lineno
          )
      | ProgFunctor v0 =>
          ( push buf "ProgFunctor" lineno
          ; push buf "(" lineno
          ; printFctBind buf v0
          ; push buf ")" lineno
          )
      | ProgSignature v0 =>
          ( push buf "ProgSignature" lineno
          ; push buf "(" lineno
          ; printSigBind buf v0
          ; push buf ")" lineno
          )
      | ProgSemicolon =>
          ( push buf "ProgSemicolon" lineno
          )
  
  and printProgList buf
    ( { node , span = { start = { lineno , ... } , ... } } : prog_list ) =
    case node of
        ProgListProgList v0 =>
          ( push buf "ProgListProgList" lineno
          ; push buf "(" lineno
          ; ( push buf "[" lineno ; List.appi (fn ( i , v0e ) => ( if i > 0 then push buf " , " lineno else () ; printProg buf v0e )) v0 ; push buf "]" lineno )
          ; push buf ")" lineno
          )
  
  and printFctBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : fct_bind ) =
    case node of
        FctBindPlain (v0 , v1 , v2 , v3 , v4 , v5) =>
          ( push buf "FctBindPlain" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printId buf v1
          ; push buf " , " lineno
          ; printSigExp buf v2
          ; push buf " , " lineno
          ; (case v3 of NONE => push buf "_" lineno | SOME v3v => printSigAnnot buf v3v)
          ; push buf " , " lineno
          ; printStr buf v4
          ; push buf " , " lineno
          ; (case v5 of NONE => push buf "_" lineno | SOME v5v => printFctBind buf v5v)
          ; push buf ")" lineno
          )
      | FctBindOpened (v0 , v1 , v2 , v3 , v4) =>
          ( push buf "FctBindOpened" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printSpec buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printSigAnnot buf v2v)
          ; push buf " , " lineno
          ; printStr buf v3
          ; push buf " , " lineno
          ; (case v4 of NONE => push buf "_" lineno | SOME v4v => printFctBind buf v4v)
          ; push buf ")" lineno
          )
  
  and printSigBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : sig_bind ) =
    case node of
        SigBindSigBind (v0 , v1 , v2) =>
          ( push buf "SigBindSigBind" lineno
          ; push buf "(" lineno
          ; printId buf v0
          ; push buf " , " lineno
          ; printSigExp buf v1
          ; push buf " , " lineno
          ; (case v2 of NONE => push buf "_" lineno | SOME v2v => printSigBind buf v2v)
          ; push buf ")" lineno
          )
  
  
  fun print f = fn v =>
  let val buf = PrintBuffer.empty ()
  in f buf v
  ; PrintBuffer.toString buf
  end
  val printChar = print printChar
  val printFloat = print printFloat
  val printId = print printId
  val printInt = print printInt
  val printString = print printString
  val printTyvar = print printTyvar
  val printWord = print printWord
  val printCon = print printCon
  val printLab = print printLab
  val printLongId = print printLongId
  val printAtomExp = print printAtomExp
  val printExp = print printExp
  val printExpListInner = print printExpListInner
  val printExpRow = print printExpRow
  val printMatch = print printMatch
  val printMatchArm = print printMatchArm
  val printPat = print printPat
  val printPatListInner = print printPatListInner
  val printPatRow = print printPatRow
  val printAtomTyp = print printAtomTyp
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
  
  
  fun prettyPrintChar buf
    ( { node , span = { start = { lineno , ... } , ... } } : char annot ) =
    push buf (Terminals.Char.show node) lineno
  
  fun prettyPrintFloat buf
    ( { node , span = { start = { lineno , ... } , ... } } : float annot ) =
    push buf (Terminals.Float.show node) lineno
  
  fun prettyPrintId buf
    ( { node , span = { start = { lineno , ... } , ... } } : id annot ) =
    push buf (Terminals.Id.show node) lineno
  
  fun prettyPrintInt buf
    ( { node , span = { start = { lineno , ... } , ... } } : int annot ) =
    push buf (Terminals.Int.show node) lineno
  
  fun prettyPrintString buf
    ( { node , span = { start = { lineno , ... } , ... } } : string annot ) =
    push buf (Terminals.String.show node) lineno
  
  fun prettyPrintTyvar buf
    ( { node , span = { start = { lineno , ... } , ... } } : tyvar annot ) =
    push buf (Terminals.Tyvar.show node) lineno
  
  fun prettyPrintWord buf
    ( { node , span = { start = { lineno , ... } , ... } } : word annot ) =
    push buf (Terminals.Word.show node) lineno
  
  fun prettyPrintCon buf
    ( { node , span = { start = { lineno , ... } , ... } } : con ) =
    let val prettyPrintSelf = prettyPrintCon
    in
    case node of
        ConInt v0 =>
          ( prettyPrintInt buf v0)
      | ConWord v0 =>
          ( prettyPrintWord buf v0)
      | ConFloat v0 =>
          ( prettyPrintFloat buf v0)
      | ConChar v0 =>
          ( prettyPrintChar buf v0)
      | ConString v0 =>
          ( prettyPrintString buf v0)
    end
  
  and prettyPrintLab buf
    ( { node , span = { start = { lineno , ... } , ... } } : lab ) =
    let val prettyPrintSelf = prettyPrintLab
    in
    case node of
        LabId v0 =>
          ( prettyPrintId buf v0)
      | LabNum v0 =>
          ( prettyPrintInt buf v0)
    end
  
  and prettyPrintLongId buf
    ( { node , span = { start = { lineno , ... } , ... } } : long_id ) =
    let val prettyPrintSelf = prettyPrintLongId
    in
    case node of
        LongIdLongId (v0 , v1) =>
          ( prettyPrintId buf v0
          ; List.app (fn v2 =>
              ( let val v4 = v2
                  in push buf "." lineno
                  ; prettyPrintId buf v4
                  end)) v1)
    end
  
  and prettyPrintAtomExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : atom_exp ) =
    let val prettyPrintSelf = prettyPrintAtomExp
    in
    case node of
        AtomExpConst v0 =>
          ( prettyPrintCon buf v0)
      | AtomExpOpId v1 =>
          ( push buf "op" lineno
          ; prettyPrintLongId buf v1)
      | AtomExpId v0 =>
          ( prettyPrintLongId buf v0)
      | AtomExpParens v1 =>
          ( push buf "(" lineno
          ; prettyPrintExp buf v1
          ; push buf ")" lineno)
      | AtomExpTuple (v1 , v3 , v4) =>
          ( push buf "(" lineno
          ; prettyPrintExp buf v1
          ; push buf "," lineno
          ; prettyPrintExp buf v3
          ; List.app (fn v5 =>
              ( let val v7 = v5
                  in push buf "," lineno
                  ; prettyPrintExp buf v7
                  end)) v4
          ; push buf ")" lineno)
      | AtomExpRecord v1 =>
          ( push buf "{" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintExpRow buf v2))
          ; push buf "}" lineno)
      | AtomExpSelector v1 =>
          ( push buf "#" lineno
          ; prettyPrintLab buf v1)
      | AtomExpList v1 =>
          ( push buf "[" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintExpListInner buf v2))
          ; push buf "]" lineno)
      | AtomExpSeq (v1 , v2) =>
          ( push buf "(" lineno
          ; prettyPrintExp buf v1
          ; List.app (fn v3 =>
              ( let val v5 = v3
                  in push buf ";" lineno
                  ; prettyPrintExp buf v5
                  end)) v2
          ; push buf ")" lineno)
      | AtomExpLet (v1 , v3 , v4) =>
          ( push buf "let" lineno
          ; prettyPrintDecList buf v1
          ; push buf "in" lineno
          ; prettyPrintExp buf v3
          ; List.app (fn v5 =>
              ( let val v7 = v5
                  in push buf ";" lineno
                  ; prettyPrintExp buf v7
                  end)) v4
          ; push buf "end" lineno)
    end
  
  and prettyPrintExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp ) =
    let val prettyPrintSelf = prettyPrintExp
    in
    case node of
        ExpApp v0 =>
          ( List.app (fn v1 =>
              ( prettyPrintAtomExp buf v1)) v0)
      | ExpCase (v1 , v3) =>
          ( push buf "case" lineno
          ; prettyPrintExp buf v1
          ; push buf "of" lineno
          ; prettyPrintMatch buf v3)
      | ExpFn v1 =>
          ( push buf "fn" lineno
          ; prettyPrintMatch buf v1)
      | ExpAnnot (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf ":" lineno
          ; prettyPrintTyp buf v2)
      | ExpHandle (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "handle" lineno
          ; prettyPrintMatch buf v2)
      | ExpRaise v1 =>
          ( push buf "raise" lineno
          ; prettyPrintSelf buf v1)
      | ExpAndAlso (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "andalso" lineno
          ; prettyPrintSelf buf v2)
      | ExpIf (v1 , v3 , v5) =>
          ( push buf "if" lineno
          ; prettyPrintExp buf v1
          ; push buf "then" lineno
          ; prettyPrintExp buf v3
          ; push buf "else" lineno
          ; prettyPrintSelf buf v5)
      | ExpWhile (v1 , v3) =>
          ( push buf "while" lineno
          ; prettyPrintExp buf v1
          ; push buf "do" lineno
          ; prettyPrintSelf buf v3)
      | ExpOrElse (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "orelse" lineno
          ; prettyPrintSelf buf v2)
    end
  
  and prettyPrintExpListInner buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp_list_inner ) =
    let val prettyPrintSelf = prettyPrintExpListInner
    in
    case node of
        ExpListInnerExpListInner (v0 , v1) =>
          ( prettyPrintExp buf v0
          ; List.app (fn v2 =>
              ( let val v4 = v2
                  in push buf "," lineno
                  ; prettyPrintExp buf v4
                  end)) v1)
    end
  
  and prettyPrintExpRow buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp_row ) =
    let val prettyPrintSelf = prettyPrintExpRow
    in
    case node of
        ExpRowExpRow (v0 , v2 , v3) =>
          ( prettyPrintLab buf v0
          ; push buf "=" lineno
          ; prettyPrintExp buf v2
          ; List.app (fn v4 =>
              ( let val (v6 , v8) = v4
                  in push buf "," lineno
                  ; prettyPrintLab buf v6
                  ; push buf "=" lineno
                  ; prettyPrintExp buf v8
                  end)) v3)
    end
  
  and prettyPrintMatch buf
    ( { node , span = { start = { lineno , ... } , ... } } : match ) =
    let val prettyPrintSelf = prettyPrintMatch
    in
    case node of
        MatchMatch (v0 , v1) =>
          ( prettyPrintMatchArm buf v0
          ; List.app (fn v2 =>
              ( let val v4 = v2
                  in push buf "|" lineno
                  ; prettyPrintMatchArm buf v4
                  end)) v1)
    end
  
  and prettyPrintMatchArm buf
    ( { node , span = { start = { lineno , ... } , ... } } : match_arm ) =
    let val prettyPrintSelf = prettyPrintMatchArm
    in
    case node of
        MatchArmMatchArm (v0 , v2) =>
          ( prettyPrintPat buf v0
          ; push buf "=>" lineno
          ; prettyPrintExp buf v2)
    end
  
  and prettyPrintPat buf
    ( { node , span = { start = { lineno , ... } , ... } } : pat ) =
    let val prettyPrintSelf = prettyPrintPat
    in
    case node of
        PatConst v0 =>
          ( prettyPrintCon buf v0)
      | PatWildcard =>
          ( push buf "_" lineno)
      | PatOpVar v1 =>
          ( push buf "op" lineno
          ; prettyPrintId buf v1)
      | PatVar v0 =>
          ( prettyPrintId buf v0)
      | PatOpCon (v1 , v2) =>
          ( push buf "op" lineno
          ; prettyPrintLongId buf v1
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( prettyPrintPat buf v3)))
      | PatParens v1 =>
          ( push buf "(" lineno
          ; prettyPrintPat buf v1
          ; push buf ")" lineno)
      | PatTuple (v1 , v3 , v4) =>
          ( push buf "(" lineno
          ; prettyPrintPat buf v1
          ; push buf "," lineno
          ; prettyPrintPat buf v3
          ; List.app (fn v5 =>
              ( let val v7 = v5
                  in push buf "," lineno
                  ; prettyPrintPat buf v7
                  end)) v4
          ; push buf ")" lineno)
      | PatRecord v1 =>
          ( push buf "{" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintPatRow buf v2))
          ; push buf "}" lineno)
      | PatList v1 =>
          ( push buf "[" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintPatListInner buf v2))
          ; push buf "]" lineno)
      | PatCon (v0 , v1) =>
          ( prettyPrintLongId buf v0
          ; prettyPrintSelf buf v1)
      | PatAnnot (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf ":" lineno
          ; prettyPrintTyp buf v2)
      | PatOpLayered (v1 , v2 , v4) =>
          ( push buf "op" lineno
          ; prettyPrintId buf v1
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf ":" lineno
                  ; prettyPrintTyp buf v5
                  end))
          ; push buf "as" lineno
          ; prettyPrintSelf buf v4)
      | PatLayered (v0 , v1 , v3) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf ":" lineno
                  ; prettyPrintTyp buf v4
                  end))
          ; push buf "as" lineno
          ; prettyPrintSelf buf v3)
    end
  
  and prettyPrintPatListInner buf
    ( { node , span = { start = { lineno , ... } , ... } } : pat_list_inner ) =
    let val prettyPrintSelf = prettyPrintPatListInner
    in
    case node of
        PatListInnerPatListInner (v0 , v1) =>
          ( prettyPrintPat buf v0
          ; List.app (fn v2 =>
              ( let val v4 = v2
                  in push buf "," lineno
                  ; prettyPrintPat buf v4
                  end)) v1)
    end
  
  and prettyPrintPatRow buf
    ( { node , span = { start = { lineno , ... } , ... } } : pat_row ) =
    let val prettyPrintSelf = prettyPrintPatRow
    in
    case node of
        PatRowWildcard =>
          ( push buf "..." lineno)
      | PatRowPat (v0 , v2 , v3) =>
          ( prettyPrintLab buf v0
          ; push buf "=" lineno
          ; prettyPrintPat buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "," lineno
                  ; prettyPrintPatRow buf v6
                  end)))
      | PatRowVar (v0 , v1 , v2 , v3) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf ":" lineno
                  ; prettyPrintTyp buf v4
                  end))
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "as" lineno
                  ; prettyPrintPat buf v5
                  end))
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "," lineno
                  ; prettyPrintPatRow buf v6
                  end)))
    end
  
  and prettyPrintAtomTyp buf
    ( { node , span = { start = { lineno , ... } , ... } } : atom_typ ) =
    let val prettyPrintSelf = prettyPrintAtomTyp
    in
    case node of
        AtomTypVar v0 =>
          ( prettyPrintTyvar buf v0)
      | AtomTypConAppMulti (v1 , v3 , v4 , v6) =>
          ( push buf "(" lineno
          ; prettyPrintTyp buf v1
          ; push buf "," lineno
          ; prettyPrintTyp buf v3
          ; List.app (fn v5 =>
              ( let val v7 = v5
                  in push buf "," lineno
                  ; prettyPrintTyp buf v7
                  end)) v4
          ; push buf ")" lineno
          ; prettyPrintLongId buf v6)
      | AtomTypCon v0 =>
          ( prettyPrintLongId buf v0)
      | AtomTypParens v1 =>
          ( push buf "(" lineno
          ; prettyPrintTyp buf v1
          ; push buf ")" lineno)
      | AtomTypRecord v1 =>
          ( push buf "{" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintTypRow buf v2))
          ; push buf "}" lineno)
      | AtomTypConApp (v0 , v1) =>
          ( prettyPrintSelf buf v0
          ; prettyPrintLongId buf v1)
    end
  
  and prettyPrintTyp buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ ) =
    let val prettyPrintSelf = prettyPrintTyp
    in
    case node of
        TypInner v0 =>
          ( prettyPrintAtomTyp buf v0)
      | TypTupleTyp (v0 , v1) =>
          ( prettyPrintAtomTyp buf v0
          ; List.app (fn v2 =>
              ( let val v4 = v2
                  in push buf "*" lineno
                  ; prettyPrintAtomTyp buf v4
                  end)) v1)
      | TypArrow (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "->" lineno
          ; prettyPrintSelf buf v2)
    end
  
  and prettyPrintTypRow buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_row ) =
    let val prettyPrintSelf = prettyPrintTypRow
    in
    case node of
        TypRowTypRow (v0 , v2 , v3) =>
          ( prettyPrintLab buf v0
          ; push buf ":" lineno
          ; prettyPrintTyp buf v2
          ; List.app (fn v4 =>
              ( let val (v6 , v8) = v4
                  in push buf "," lineno
                  ; prettyPrintLab buf v6
                  ; push buf ":" lineno
                  ; prettyPrintTyp buf v8
                  end)) v3)
    end
  
  and prettyPrintDec buf
    ( { node , span = { start = { lineno , ... } , ... } } : dec ) =
    let val prettyPrintSelf = prettyPrintDec
    in
    case node of
        DecVal (v1 , v2) =>
          ( push buf "val" lineno
          ; prettyPrintTyVarSeq buf v1
          ; prettyPrintValBind buf v2)
      | DecFun (v1 , v2) =>
          ( push buf "fun" lineno
          ; prettyPrintTyVarSeq buf v1
          ; prettyPrintFunBind buf v2)
      | DecType v1 =>
          ( push buf "type" lineno
          ; prettyPrintTypBind buf v1)
      | DecDatatype (v1 , v2) =>
          ( push buf "datatype" lineno
          ; prettyPrintDatBind buf v1
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "withtype" lineno
                  ; prettyPrintTypBind buf v5
                  end)))
      | DecDatatypeRepl (v1 , v4) =>
          ( push buf "datatype" lineno
          ; prettyPrintId buf v1
          ; push buf "=" lineno
          ; push buf "datatype" lineno
          ; prettyPrintLongId buf v4)
      | DecAbstype (v1 , v2 , v4) =>
          ( push buf "abstype" lineno
          ; prettyPrintDatBind buf v1
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "withtype" lineno
                  ; prettyPrintTypBind buf v5
                  end))
          ; push buf "with" lineno
          ; prettyPrintDecList buf v4
          ; push buf "end" lineno)
      | DecException v1 =>
          ( push buf "exception" lineno
          ; prettyPrintExnBind buf v1)
      | DecStructure v1 =>
          ( push buf "structure" lineno
          ; prettyPrintStrBind buf v1)
      | DecSemicolon =>
          ( push buf ";" lineno)
      | DecLocal (v1 , v3) =>
          ( push buf "local" lineno
          ; prettyPrintDecList buf v1
          ; push buf "in" lineno
          ; prettyPrintDecList buf v3
          ; push buf "end" lineno)
      | DecOpen (v1 , v2) =>
          ( push buf "open" lineno
          ; prettyPrintLongId buf v1
          ; List.app (fn v3 =>
              ( prettyPrintLongId buf v3)) v2)
      | DecNonfix (v1 , v2) =>
          ( push buf "nonfix" lineno
          ; prettyPrintId buf v1
          ; List.app (fn v3 =>
              ( prettyPrintId buf v3)) v2)
      | DecInfix (v1 , v2 , v3) =>
          ( push buf "infix" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintInt buf v2))
          ; prettyPrintId buf v2
          ; List.app (fn v4 =>
              ( prettyPrintId buf v4)) v3)
      | DecInfixr (v1 , v2 , v3) =>
          ( push buf "infixr" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintInt buf v2))
          ; prettyPrintId buf v2
          ; List.app (fn v4 =>
              ( prettyPrintId buf v4)) v3)
    end
  
  and prettyPrintDecList buf
    ( { node , span = { start = { lineno , ... } , ... } } : dec_list ) =
    let val prettyPrintSelf = prettyPrintDecList
    in
    case node of
        DecListDecList v0 =>
          ( List.app (fn v1 =>
              ( prettyPrintDec buf v1)) v0)
    end
  
  and prettyPrintTyVarSeq buf
    ( { node , span = { start = { lineno , ... } , ... } } : ty_var_seq ) =
    let val prettyPrintSelf = prettyPrintTyVarSeq
    in
    case node of
        TyVarSeqOne v0 =>
          ( prettyPrintTyvar buf v0)
      | TyVarSeqMany (v1 , v2) =>
          ( push buf "(" lineno
          ; prettyPrintTyvar buf v1
          ; List.app (fn v3 =>
              ( let val v5 = v3
                  in push buf "," lineno
                  ; prettyPrintTyvar buf v5
                  end)) v2
          ; push buf ")" lineno)
      | TyVarSeqEmpty =>
          ( )
    end
  
  and prettyPrintValBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : val_bind ) =
    let val prettyPrintSelf = prettyPrintValBind
    in
    case node of
        ValBindValBind (v0 , v2 , v3) =>
          ( prettyPrintPat buf v0
          ; push buf "=" lineno
          ; prettyPrintExp buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "and" lineno
                  ; prettyPrintValBind buf v6
                  end)))
      | ValBindRec v1 =>
          ( push buf "rec" lineno
          ; prettyPrintSelf buf v1)
    end
  
  and prettyPrintFunBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : fun_bind ) =
    let val prettyPrintSelf = prettyPrintFunBind
    in
    case node of
        FunBindFunBind (v0 , v1) =>
          ( prettyPrintFunMatch buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf "and" lineno
                  ; prettyPrintFunBind buf v4
                  end)))
    end
  
  and prettyPrintFunMatch buf
    ( { node , span = { start = { lineno , ... } , ... } } : fun_match ) =
    let val prettyPrintSelf = prettyPrintFunMatch
    in
    case node of
        FunMatchNonfix (v1 , v2 , v3 , v4 , v6 , v7) =>
          ( prettyPrintId buf v1
          ; prettyPrintPat buf v2
          ; List.app (fn v4 =>
              ( prettyPrintPat buf v4)) v3
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( let val v7 = v5
                  in push buf ":" lineno
                  ; prettyPrintTyp buf v7
                  end))
          ; push buf "=" lineno
          ; prettyPrintExp buf v6
          ; (case v7 of NONE => ()
              | SOME v8 =>
              ( let val v10 = v8
                  in push buf "|" lineno
                  ; prettyPrintFunMatch buf v10
                  end)))
      | FunMatchInfix (v0 , v1 , v2 , v3 , v5 , v6) =>
          ( prettyPrintPat buf v0
          ; prettyPrintId buf v1
          ; prettyPrintPat buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf ":" lineno
                  ; prettyPrintTyp buf v6
                  end))
          ; push buf "=" lineno
          ; prettyPrintExp buf v5
          ; (case v6 of NONE => ()
              | SOME v7 =>
              ( let val v9 = v7
                  in push buf "|" lineno
                  ; prettyPrintFunMatch buf v9
                  end)))
      | FunMatchInfixParen (v1 , v2 , v3 , v5 , v6 , v8 , v9) =>
          ( push buf "(" lineno
          ; prettyPrintPat buf v1
          ; prettyPrintId buf v2
          ; prettyPrintPat buf v3
          ; push buf ")" lineno
          ; List.app (fn v6 =>
              ( prettyPrintPat buf v6)) v5
          ; (case v6 of NONE => ()
              | SOME v7 =>
              ( let val v9 = v7
                  in push buf ":" lineno
                  ; prettyPrintTyp buf v9
                  end))
          ; push buf "=" lineno
          ; prettyPrintExp buf v8
          ; (case v9 of NONE => ()
              | SOME v10 =>
              ( let val v12 = v10
                  in push buf "|" lineno
                  ; prettyPrintFunMatch buf v12
                  end)))
    end
  
  and prettyPrintTypBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_bind ) =
    let val prettyPrintSelf = prettyPrintTypBind
    in
    case node of
        TypBindTypBind (v0 , v1 , v3 , v4) =>
          ( prettyPrintTyVarSeq buf v0
          ; prettyPrintId buf v1
          ; push buf "=" lineno
          ; prettyPrintTyp buf v3
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( let val v7 = v5
                  in push buf "and" lineno
                  ; prettyPrintTypBind buf v7
                  end)))
    end
  
  and prettyPrintDatBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : dat_bind ) =
    let val prettyPrintSelf = prettyPrintDatBind
    in
    case node of
        DatBindDatBind (v0 , v1 , v3 , v4) =>
          ( prettyPrintTyVarSeq buf v0
          ; prettyPrintId buf v1
          ; push buf "=" lineno
          ; prettyPrintConBind buf v3
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( let val v7 = v5
                  in push buf "and" lineno
                  ; prettyPrintDatBind buf v7
                  end)))
    end
  
  and prettyPrintConBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : con_bind ) =
    let val prettyPrintSelf = prettyPrintConBind
    in
    case node of
        ConBindConBind (v0 , v1 , v2) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf "of" lineno
                  ; prettyPrintTyp buf v4
                  end))
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "|" lineno
                  ; prettyPrintConBind buf v5
                  end)))
    end
  
  and prettyPrintExnBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : exn_bind ) =
    let val prettyPrintSelf = prettyPrintExnBind
    in
    case node of
        ExnBindGen (v0 , v1 , v2) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf "of" lineno
                  ; prettyPrintTyp buf v4
                  end))
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "and" lineno
                  ; prettyPrintExnBind buf v5
                  end)))
      | ExnBindRepl (v0 , v2 , v3) =>
          ( prettyPrintId buf v0
          ; push buf "=" lineno
          ; prettyPrintLongId buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "and" lineno
                  ; prettyPrintExnBind buf v6
                  end)))
    end
  
  and prettyPrintStr buf
    ( { node , span = { start = { lineno , ... } , ... } } : str ) =
    let val prettyPrintSelf = prettyPrintStr
    in
    case node of
        StrId v0 =>
          ( prettyPrintLongId buf v0)
      | StrStruct v1 =>
          ( push buf "struct" lineno
          ; prettyPrintDecList buf v1
          ; push buf "end" lineno)
      | StrFctApp (v0 , v2) =>
          ( prettyPrintId buf v0
          ; push buf "(" lineno
          ; prettyPrintStr buf v2
          ; push buf ")" lineno)
      | StrFctAppDec (v0 , v2) =>
          ( prettyPrintId buf v0
          ; push buf "(" lineno
          ; prettyPrintDecList buf v2
          ; push buf ")" lineno)
      | StrLet (v1 , v3) =>
          ( push buf "let" lineno
          ; prettyPrintDecList buf v1
          ; push buf "in" lineno
          ; prettyPrintStr buf v3
          ; push buf "end" lineno)
      | StrTransparent (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf ":" lineno
          ; prettyPrintSigExp buf v2)
      | StrOpaque (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf ":>" lineno
          ; prettyPrintSigExp buf v2)
    end
  
  and prettyPrintStrBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : str_bind ) =
    let val prettyPrintSelf = prettyPrintStrBind
    in
    case node of
        StrBindStrBind (v0 , v1 , v3 , v4) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( prettyPrintSigAnnot buf v2))
          ; push buf "=" lineno
          ; prettyPrintStr buf v3
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( let val v7 = v5
                  in push buf "and" lineno
                  ; prettyPrintStrBind buf v7
                  end)))
    end
  
  and prettyPrintSigAnnot buf
    ( { node , span = { start = { lineno , ... } , ... } } : sig_annot ) =
    let val prettyPrintSelf = prettyPrintSigAnnot
    in
    case node of
        SigAnnotTransparent v1 =>
          ( push buf ":" lineno
          ; prettyPrintSigExp buf v1)
      | SigAnnotOpaque v1 =>
          ( push buf ":>" lineno
          ; prettyPrintSigExp buf v1)
    end
  
  and prettyPrintSigExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : sig_exp ) =
    let val prettyPrintSelf = prettyPrintSigExp
    in
    case node of
        SigExpId v0 =>
          ( prettyPrintId buf v0)
      | SigExpSig v1 =>
          ( push buf "sig" lineno
          ; prettyPrintSpecList buf v1
          ; push buf "end" lineno)
      | SigExpWhere (v0 , v3) =>
          ( prettyPrintSelf buf v0
          ; push buf "where" lineno
          ; push buf "type" lineno
          ; prettyPrintTypRefin buf v3)
    end
  
  and prettyPrintTypRefin buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_refin ) =
    let val prettyPrintSelf = prettyPrintTypRefin
    in
    case node of
        TypRefinTypRefin (v0 , v1 , v3 , v4) =>
          ( prettyPrintTyVarSeq buf v0
          ; prettyPrintLongId buf v1
          ; push buf "=" lineno
          ; prettyPrintTyp buf v3
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( let val v8 = v5
                  in push buf "and" lineno
                  ; push buf "type" lineno
                  ; prettyPrintTypRefin buf v8
                  end)))
    end
  
  and prettyPrintSpec buf
    ( { node , span = { start = { lineno , ... } , ... } } : spec ) =
    let val prettyPrintSelf = prettyPrintSpec
    in
    case node of
        SpecVal v1 =>
          ( push buf "val" lineno
          ; prettyPrintValDesc buf v1)
      | SpecType v1 =>
          ( push buf "type" lineno
          ; prettyPrintTypDesc buf v1)
      | SpecEqtype v1 =>
          ( push buf "eqtype" lineno
          ; prettyPrintTypDesc buf v1)
      | SpecTypeAbbrev v1 =>
          ( push buf "type" lineno
          ; prettyPrintTypBind buf v1)
      | SpecDatatype v1 =>
          ( push buf "datatype" lineno
          ; prettyPrintDatDesc buf v1)
      | SpecDatatypeRepl (v1 , v4) =>
          ( push buf "datatype" lineno
          ; prettyPrintId buf v1
          ; push buf "=" lineno
          ; push buf "datatype" lineno
          ; prettyPrintLongId buf v4)
      | SpecException v1 =>
          ( push buf "exception" lineno
          ; prettyPrintExnDesc buf v1)
      | SpecStructure v1 =>
          ( push buf "structure" lineno
          ; prettyPrintStrDesc buf v1)
      | SpecSemicolon =>
          ( push buf ";" lineno)
      | SpecInclude v1 =>
          ( push buf "include" lineno
          ; prettyPrintSigExp buf v1)
      | SpecIncludeMulti (v1 , v2) =>
          ( push buf "include" lineno
          ; prettyPrintId buf v1
          ; List.app (fn v3 =>
              ( prettyPrintId buf v3)) v2)
      | SpecSharingType (v0 , v3 , v4) =>
          ( prettyPrintSelf buf v0
          ; push buf "sharing" lineno
          ; push buf "type" lineno
          ; prettyPrintLongId buf v3
          ; List.app (fn v5 =>
              ( let val v7 = v5
                  in push buf "=" lineno
                  ; prettyPrintLongId buf v7
                  end)) v4)
      | SpecSharing (v0 , v2 , v3) =>
          ( prettyPrintSelf buf v0
          ; push buf "sharing" lineno
          ; prettyPrintLongId buf v2
          ; List.app (fn v4 =>
              ( let val v6 = v4
                  in push buf "=" lineno
                  ; prettyPrintLongId buf v6
                  end)) v3)
    end
  
  and prettyPrintSpecList buf
    ( { node , span = { start = { lineno , ... } , ... } } : spec_list ) =
    let val prettyPrintSelf = prettyPrintSpecList
    in
    case node of
        SpecListSpecList v0 =>
          ( List.app (fn v1 =>
              ( prettyPrintSpec buf v1)) v0)
    end
  
  and prettyPrintValDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : val_desc ) =
    let val prettyPrintSelf = prettyPrintValDesc
    in
    case node of
        ValDescValDesc (v0 , v2 , v3) =>
          ( prettyPrintId buf v0
          ; push buf ":" lineno
          ; prettyPrintTyp buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "and" lineno
                  ; prettyPrintValDesc buf v6
                  end)))
    end
  
  and prettyPrintTypDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : typ_desc ) =
    let val prettyPrintSelf = prettyPrintTypDesc
    in
    case node of
        TypDescTypDesc (v0 , v1 , v2) =>
          ( prettyPrintTyVarSeq buf v0
          ; prettyPrintId buf v1
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "and" lineno
                  ; prettyPrintTypDesc buf v5
                  end)))
    end
  
  and prettyPrintDatDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : dat_desc ) =
    let val prettyPrintSelf = prettyPrintDatDesc
    in
    case node of
        DatDescDatDesc (v0 , v1 , v3 , v4) =>
          ( prettyPrintTyVarSeq buf v0
          ; prettyPrintId buf v1
          ; push buf "=" lineno
          ; prettyPrintConDesc buf v3
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( let val v7 = v5
                  in push buf "and" lineno
                  ; prettyPrintDatDesc buf v7
                  end)))
    end
  
  and prettyPrintConDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : con_desc ) =
    let val prettyPrintSelf = prettyPrintConDesc
    in
    case node of
        ConDescConDesc (v0 , v1 , v2) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf "of" lineno
                  ; prettyPrintTyp buf v4
                  end))
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "|" lineno
                  ; prettyPrintConDesc buf v5
                  end)))
    end
  
  and prettyPrintExnDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : exn_desc ) =
    let val prettyPrintSelf = prettyPrintExnDesc
    in
    case node of
        ExnDescExnDesc (v0 , v1 , v2) =>
          ( prettyPrintId buf v0
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val v4 = v2
                  in push buf "of" lineno
                  ; prettyPrintTyp buf v4
                  end))
          ; (case v2 of NONE => ()
              | SOME v3 =>
              ( let val v5 = v3
                  in push buf "and" lineno
                  ; prettyPrintExnDesc buf v5
                  end)))
    end
  
  and prettyPrintStrDesc buf
    ( { node , span = { start = { lineno , ... } , ... } } : str_desc ) =
    let val prettyPrintSelf = prettyPrintStrDesc
    in
    case node of
        StrDescStrDesc (v0 , v2 , v3) =>
          ( prettyPrintId buf v0
          ; push buf ":" lineno
          ; prettyPrintSigExp buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "and" lineno
                  ; prettyPrintStrDesc buf v6
                  end)))
    end
  
  and prettyPrintProg buf
    ( { node , span = { start = { lineno , ... } , ... } } : prog ) =
    let val prettyPrintSelf = prettyPrintProg
    in
    case node of
        ProgDec v0 =>
          ( prettyPrintDec buf v0)
      | ProgFunctor v1 =>
          ( push buf "functor" lineno
          ; prettyPrintFctBind buf v1)
      | ProgSignature v1 =>
          ( push buf "signature" lineno
          ; prettyPrintSigBind buf v1)
      | ProgSemicolon =>
          ( push buf ";" lineno)
    end
  
  and prettyPrintProgList buf
    ( { node , span = { start = { lineno , ... } , ... } } : prog_list ) =
    let val prettyPrintSelf = prettyPrintProgList
    in
    case node of
        ProgListProgList v0 =>
          ( List.app (fn v1 =>
              ( prettyPrintProg buf v1)) v0)
    end
  
  and prettyPrintFctBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : fct_bind ) =
    let val prettyPrintSelf = prettyPrintFctBind
    in
    case node of
        FctBindPlain (v0 , v2 , v4 , v6 , v8 , v9) =>
          ( prettyPrintId buf v0
          ; push buf "(" lineno
          ; prettyPrintId buf v2
          ; push buf ":" lineno
          ; prettyPrintSigExp buf v4
          ; push buf ")" lineno
          ; (case v6 of NONE => ()
              | SOME v7 =>
              ( prettyPrintSigAnnot buf v7))
          ; push buf "=" lineno
          ; prettyPrintStr buf v8
          ; (case v9 of NONE => ()
              | SOME v10 =>
              ( let val v12 = v10
                  in push buf "and" lineno
                  ; prettyPrintFctBind buf v12
                  end)))
      | FctBindOpened (v0 , v2 , v4 , v6 , v7) =>
          ( prettyPrintId buf v0
          ; push buf "(" lineno
          ; prettyPrintSpec buf v2
          ; push buf ")" lineno
          ; (case v4 of NONE => ()
              | SOME v5 =>
              ( prettyPrintSigAnnot buf v5))
          ; push buf "=" lineno
          ; prettyPrintStr buf v6
          ; (case v7 of NONE => ()
              | SOME v8 =>
              ( let val v10 = v8
                  in push buf "and" lineno
                  ; prettyPrintFctBind buf v10
                  end)))
    end
  
  and prettyPrintSigBind buf
    ( { node , span = { start = { lineno , ... } , ... } } : sig_bind ) =
    let val prettyPrintSelf = prettyPrintSigBind
    in
    case node of
        SigBindSigBind (v0 , v2 , v3) =>
          ( prettyPrintId buf v0
          ; push buf "=" lineno
          ; prettyPrintSigExp buf v2
          ; (case v3 of NONE => ()
              | SOME v4 =>
              ( let val v6 = v4
                  in push buf "and" lineno
                  ; prettyPrintSigBind buf v6
                  end)))
    end
  
  
  val prettyPrintChar = print prettyPrintChar
  val prettyPrintFloat = print prettyPrintFloat
  val prettyPrintId = print prettyPrintId
  val prettyPrintInt = print prettyPrintInt
  val prettyPrintString = print prettyPrintString
  val prettyPrintTyvar = print prettyPrintTyvar
  val prettyPrintWord = print prettyPrintWord
  val prettyPrintCon = print prettyPrintCon
  val prettyPrintLab = print prettyPrintLab
  val prettyPrintLongId = print prettyPrintLongId
  val prettyPrintAtomExp = print prettyPrintAtomExp
  val prettyPrintExp = print prettyPrintExp
  val prettyPrintExpListInner = print prettyPrintExpListInner
  val prettyPrintExpRow = print prettyPrintExpRow
  val prettyPrintMatch = print prettyPrintMatch
  val prettyPrintMatchArm = print prettyPrintMatchArm
  val prettyPrintPat = print prettyPrintPat
  val prettyPrintPatListInner = print prettyPrintPatListInner
  val prettyPrintPatRow = print prettyPrintPatRow
  val prettyPrintAtomTyp = print prettyPrintAtomTyp
  val prettyPrintTyp = print prettyPrintTyp
  val prettyPrintTypRow = print prettyPrintTypRow
  val prettyPrintDec = print prettyPrintDec
  val prettyPrintDecList = print prettyPrintDecList
  val prettyPrintTyVarSeq = print prettyPrintTyVarSeq
  val prettyPrintValBind = print prettyPrintValBind
  val prettyPrintFunBind = print prettyPrintFunBind
  val prettyPrintFunMatch = print prettyPrintFunMatch
  val prettyPrintTypBind = print prettyPrintTypBind
  val prettyPrintDatBind = print prettyPrintDatBind
  val prettyPrintConBind = print prettyPrintConBind
  val prettyPrintExnBind = print prettyPrintExnBind
  val prettyPrintStr = print prettyPrintStr
  val prettyPrintStrBind = print prettyPrintStrBind
  val prettyPrintSigAnnot = print prettyPrintSigAnnot
  val prettyPrintSigExp = print prettyPrintSigExp
  val prettyPrintTypRefin = print prettyPrintTypRefin
  val prettyPrintSpec = print prettyPrintSpec
  val prettyPrintSpecList = print prettyPrintSpecList
  val prettyPrintValDesc = print prettyPrintValDesc
  val prettyPrintTypDesc = print prettyPrintTypDesc
  val prettyPrintDatDesc = print prettyPrintDatDesc
  val prettyPrintConDesc = print prettyPrintConDesc
  val prettyPrintExnDesc = print prettyPrintExnDesc
  val prettyPrintStrDesc = print prettyPrintStrDesc
  val prettyPrintProg = print prettyPrintProg
  val prettyPrintProgList = print prettyPrintProgList
  val prettyPrintFctBind = print prettyPrintFctBind
  val prettyPrintSigBind = print prettyPrintSigBind
  
end

functor SmlRepl (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Char : REPL_TERMINAL
    structure Float : REPL_TERMINAL
    structure Id : REPL_TERMINAL
    structure Int : REPL_TERMINAL
    structure String : REPL_TERMINAL
    structure Tyvar : REPL_TERMINAL
    structure Word : REPL_TERMINAL
  end
) :> sig val run : unit -> unit end = struct

  structure Parser = SmlParser (
    structure Trivial = Trivial
    structure Terminals = Terminals
  )
  
  structure Print = SmlPrint (
    structure Ast = Parser
    structure Terminals = Terminals
  )
  
  structure Repl = Repl (
    structure Result = struct
      type t = Parser.sig_bind
      type token_stream = Parser.token_stream
      exception LexError = Parser.LexError
      val lex = Parser.lex
      val parse = Parser.parse Parser.parseSigBind
      val print = Print.printSigBind
    end
  )
  
  val run = Repl.run
end