structure ParseTokens =
  struct
    datatype token
      = COLONCOLONEQ
      | BAR
      | STAR
      | QUERY
      | LP
      | RP
      | LB
      | RB
      | COMMA
      | ASSOC_LEFT
      | ASSOC_RIGHT
      | ASSOC_NONE
      | RULE_NAME
      | KW_ASSOC
      | KW_PREC
      | STRING of string
      | TERMINAL of string
      | INT of int
      | IDENT of string
      | EOF
    val allToks = [
            COLONCOLONEQ, BAR, STAR, QUERY, LP, RP, LB, RB, COMMA, ASSOC_LEFT, ASSOC_RIGHT, ASSOC_NONE, RULE_NAME, KW_ASSOC, KW_PREC, EOF
           ]
    fun toString tok =
(case (tok)
 of (COLONCOLONEQ) => "::="
  | (BAR) => "|"
  | (STAR) => "*"
  | (QUERY) => "?"
  | (LP) => "("
  | (RP) => ")"
  | (LB) => "["
  | (RB) => "]"
  | (COMMA) => ","
  | (ASSOC_LEFT) => "left"
  | (ASSOC_RIGHT) => "right"
  | (ASSOC_NONE) => "none"
  | (RULE_NAME) => "name"
  | (KW_ASSOC) => "assoc"
  | (KW_PREC) => "prec"
  | (STRING(_)) => "STRING"
  | (TERMINAL(_)) => "TERMINAL"
  | (INT(_)) => "INT"
  | (IDENT(_)) => "IDENT"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (COLONCOLONEQ) => false
  | (BAR) => false
  | (STAR) => false
  | (QUERY) => false
  | (LP) => false
  | (RP) => false
  | (LB) => false
  | (RB) => false
  | (COMMA) => false
  | (ASSOC_LEFT) => false
  | (ASSOC_RIGHT) => false
  | (ASSOC_NONE) => false
  | (RULE_NAME) => false
  | (KW_ASSOC) => false
  | (KW_PREC) => false
  | (STRING(_)) => false
  | (TERMINAL(_)) => false
  | (INT(_)) => false
  | (IDENT(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* ParseTokens *)

functor ParseParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
ParseTokens
    structure UserCode =
      struct

  structure A = Ast

fun Grammar_PROD_1_ACT (Definition, Definition_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({ definitions = Definition } : A.grammar)
fun Definition_PROD_1_ACT (SR, COLONCOLONEQ, IDENT, Alt, SR_SPAN : (Lex.pos * Lex.pos), COLONCOLONEQ_SPAN : (Lex.pos * Lex.pos), IDENT_SPAN : (Lex.pos * Lex.pos), Alt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({ name = IDENT , rule = Alt :: SR } : A.definition)
fun Alt_PROD_1_ACT (LB, SR, RB, Property, SeqRule, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Property_SPAN : (Lex.pos * Lex.pos), SeqRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SeqRule , Property :: SR)
fun SeqRule_PROD_1_ACT (PostfixRule, PostfixRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case PostfixRule
                       of [single] => single
                        | items => A.Seq items)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_1_ACT (AtomRule, STAR, AtomRule_SPAN : (Lex.pos * Lex.pos), STAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Star AtomRule)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_2_ACT (QUERY, AtomRule, QUERY_SPAN : (Lex.pos * Lex.pos), AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Opt AtomRule)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_3_ACT (AtomRule, AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomRule)
fun PostfixRule_PROD_1_ACT (SR, AtomRule, SR_SPAN : (Lex.pos * Lex.pos), AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SR)
fun AtomRule_PROD_1_ACT (IDENT, IDENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Nonterminal IDENT)
fun AtomRule_PROD_2_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Keyword STRING)
fun AtomRule_PROD_3_ACT (TERMINAL, TERMINAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Terminal TERMINAL)
fun AtomRule_PROD_4_ACT (LP, RP, SeqRule, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SeqRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SeqRule)
fun Property_PROD_1_ACT (Assoc, KW_ASSOC, Assoc_SPAN : (Lex.pos * Lex.pos), KW_ASSOC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Assoc Assoc)
fun Property_PROD_2_ACT (KW_PREC, INT, KW_PREC_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Prec INT)
fun Property_PROD_3_ACT (RULE_NAME, IDENT, RULE_NAME_SPAN : (Lex.pos * Lex.pos), IDENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.RuleName IDENT)
fun Assoc_PROD_1_ACT (ASSOC_LEFT, ASSOC_LEFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Left)
fun Assoc_PROD_2_ACT (ASSOC_RIGHT, ASSOC_RIGHT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Right)
fun Assoc_PROD_3_ACT (ASSOC_NONE, ASSOC_NONE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.None)
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchCOLONCOLONEQ strm = (case (lex(strm))
 of (Tok.COLONCOLONEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBAR strm = (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTAR strm = (case (lex(strm))
 of (Tok.STAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchQUERY strm = (case (lex(strm))
 of (Tok.QUERY, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSOC_LEFT strm = (case (lex(strm))
 of (Tok.ASSOC_LEFT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSOC_RIGHT strm = (case (lex(strm))
 of (Tok.ASSOC_RIGHT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSOC_NONE strm = (case (lex(strm))
 of (Tok.ASSOC_NONE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRULE_NAME strm = (case (lex(strm))
 of (Tok.RULE_NAME, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_ASSOC strm = (case (lex(strm))
 of (Tok.KW_ASSOC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_PREC strm = (case (lex(strm))
 of (Tok.KW_PREC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchTERMINAL strm = (case (lex(strm))
 of (Tok.TERMINAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchINT strm = (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchIDENT strm = (case (lex(strm))
 of (Tok.IDENT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Grammar_NT) = 
let
fun Assoc_NT (strm) = let
      fun Assoc_PROD_1 (strm) = let
            val (ASSOC_LEFT_RES, ASSOC_LEFT_SPAN, strm') = matchASSOC_LEFT(strm)
            val FULL_SPAN = (#1(ASSOC_LEFT_SPAN), #2(ASSOC_LEFT_SPAN))
            in
              (UserCode.Assoc_PROD_1_ACT (ASSOC_LEFT_RES, ASSOC_LEFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Assoc_PROD_2 (strm) = let
            val (ASSOC_RIGHT_RES, ASSOC_RIGHT_SPAN, strm') = matchASSOC_RIGHT(strm)
            val FULL_SPAN = (#1(ASSOC_RIGHT_SPAN), #2(ASSOC_RIGHT_SPAN))
            in
              (UserCode.Assoc_PROD_2_ACT (ASSOC_RIGHT_RES, ASSOC_RIGHT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Assoc_PROD_3 (strm) = let
            val (ASSOC_NONE_RES, ASSOC_NONE_SPAN, strm') = matchASSOC_NONE(strm)
            val FULL_SPAN = (#1(ASSOC_NONE_SPAN), #2(ASSOC_NONE_SPAN))
            in
              (UserCode.Assoc_PROD_3_ACT (ASSOC_NONE_RES, ASSOC_NONE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ASSOC_NONE, _, strm') => Assoc_PROD_3(strm)
          | (Tok.ASSOC_LEFT, _, strm') => Assoc_PROD_1(strm)
          | (Tok.ASSOC_RIGHT, _, strm') => Assoc_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun Property_NT (strm) = let
      fun Property_PROD_1 (strm) = let
            val (KW_ASSOC_RES, KW_ASSOC_SPAN, strm') = matchKW_ASSOC(strm)
            val (Assoc_RES, Assoc_SPAN, strm') = Assoc_NT(strm')
            val FULL_SPAN = (#1(KW_ASSOC_SPAN), #2(Assoc_SPAN))
            in
              (UserCode.Property_PROD_1_ACT (Assoc_RES, KW_ASSOC_RES, Assoc_SPAN : (Lex.pos * Lex.pos), KW_ASSOC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Property_PROD_2 (strm) = let
            val (KW_PREC_RES, KW_PREC_SPAN, strm') = matchKW_PREC(strm)
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val FULL_SPAN = (#1(KW_PREC_SPAN), #2(INT_SPAN))
            in
              (UserCode.Property_PROD_2_ACT (KW_PREC_RES, INT_RES, KW_PREC_SPAN : (Lex.pos * Lex.pos), INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Property_PROD_3 (strm) = let
            val (RULE_NAME_RES, RULE_NAME_SPAN, strm') = matchRULE_NAME(strm)
            val (IDENT_RES, IDENT_SPAN, strm') = matchIDENT(strm')
            val FULL_SPAN = (#1(RULE_NAME_SPAN), #2(IDENT_SPAN))
            in
              (UserCode.Property_PROD_3_ACT (RULE_NAME_RES, IDENT_RES, RULE_NAME_SPAN : (Lex.pos * Lex.pos), IDENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.RULE_NAME, _, strm') => Property_PROD_3(strm)
          | (Tok.KW_ASSOC, _, strm') => Property_PROD_1(strm)
          | (Tok.KW_PREC, _, strm') => Property_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun SeqRule_NT (strm) = let
      fun SeqRule_PROD_1_SUBRULE_1_NT (strm) = let
            val (PostfixRule_RES, PostfixRule_SPAN, strm') = PostfixRule_NT(strm)
            val FULL_SPAN = (#1(PostfixRule_SPAN), #2(PostfixRule_SPAN))
            in
              ((PostfixRule_RES), FULL_SPAN, strm')
            end
      fun SeqRule_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LP, _, strm') => true
              | (Tok.STRING(_), _, strm') => true
              | (Tok.TERMINAL(_), _, strm') => true
              | (Tok.IDENT(_), _, strm') => true
              | _ => false
            (* end case *))
      val (PostfixRule_RES, PostfixRule_SPAN, strm') = EBNF.posclos(SeqRule_PROD_1_SUBRULE_1_PRED, SeqRule_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(PostfixRule_SPAN), #2(PostfixRule_SPAN))
      in
        (UserCode.SeqRule_PROD_1_ACT (PostfixRule_RES, PostfixRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and PostfixRule_NT (strm) = let
      val (AtomRule_RES, AtomRule_SPAN, strm') = AtomRule_NT(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun PostfixRule_PROD_1_SUBRULE_1_NT (strm) = let
            fun PostfixRule_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (STAR_RES, STAR_SPAN, strm') = matchSTAR(strm)
                  val FULL_SPAN = (#1(STAR_SPAN), #2(STAR_SPAN))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_1_ACT (AtomRule_RES, STAR_RES, AtomRule_SPAN : (Lex.pos * Lex.pos), STAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun PostfixRule_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (QUERY_RES, QUERY_SPAN, strm') = matchQUERY(strm)
                  val FULL_SPAN = (#1(QUERY_SPAN), #2(QUERY_SPAN))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_2_ACT (QUERY_RES, AtomRule_RES, QUERY_SPAN : (Lex.pos * Lex.pos), AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun PostfixRule_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_3_ACT (AtomRule_RES, AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm)
                  end
            in
              (case (lex(strm))
               of (Tok.LP, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.RP, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.LB, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.STRING(_), _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.TERMINAL(_), _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.IDENT(_), _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
                | (Tok.STAR, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.QUERY, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_2(strm)
                | _ => fail()
              (* end case *))
            end
      in
        PostfixRule_PROD_1_SUBRULE_1_NT(strm')
      end
      val FULL_SPAN = (#1(AtomRule_SPAN), #2(SR_SPAN))
      in
        (UserCode.PostfixRule_PROD_1_ACT (SR_RES, AtomRule_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AtomRule_NT (strm) = let
      fun AtomRule_PROD_1 (strm) = let
            val (IDENT_RES, IDENT_SPAN, strm') = matchIDENT(strm)
            val FULL_SPAN = (#1(IDENT_SPAN), #2(IDENT_SPAN))
            in
              (UserCode.AtomRule_PROD_1_ACT (IDENT_RES, IDENT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomRule_PROD_2 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomRule_PROD_2_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomRule_PROD_3 (strm) = let
            val (TERMINAL_RES, TERMINAL_SPAN, strm') = matchTERMINAL(strm)
            val FULL_SPAN = (#1(TERMINAL_SPAN), #2(TERMINAL_SPAN))
            in
              (UserCode.AtomRule_PROD_3_ACT (TERMINAL_RES, TERMINAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomRule_PROD_4 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (SeqRule_RES, SeqRule_SPAN, strm') = SeqRule_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomRule_PROD_4_ACT (LP_RES, RP_RES, SeqRule_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SeqRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => AtomRule_PROD_4(strm)
          | (Tok.STRING(_), _, strm') => AtomRule_PROD_2(strm)
          | (Tok.IDENT(_), _, strm') => AtomRule_PROD_1(strm)
          | (Tok.TERMINAL(_), _, strm') => AtomRule_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun Alt_NT (strm) = let
      val (SeqRule_RES, SeqRule_SPAN, strm') = SeqRule_NT(strm)
      val (LB_RES, LB_SPAN, strm') = matchLB(strm')
      val (Property_RES, Property_SPAN, strm') = Property_NT(strm')
      fun Alt_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Property_RES, Property_SPAN, strm') = Property_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Property_SPAN))
            in
              ((Property_RES), FULL_SPAN, strm')
            end
      fun Alt_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Alt_PROD_1_SUBRULE_1_PRED, Alt_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(SeqRule_SPAN), #2(RB_SPAN))
      in
        (UserCode.Alt_PROD_1_ACT (LB_RES, SR_RES, RB_RES, Property_RES, SeqRule_RES, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Property_SPAN : (Lex.pos * Lex.pos), SeqRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Definition_NT (strm) = let
      val (IDENT_RES, IDENT_SPAN, strm') = matchIDENT(strm)
      val (COLONCOLONEQ_RES, COLONCOLONEQ_SPAN, strm') = matchCOLONCOLONEQ(strm')
      val (Alt_RES, Alt_SPAN, strm') = Alt_NT(strm')
      fun Definition_PROD_1_SUBRULE_1_NT (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (Alt_RES, Alt_SPAN, strm') = Alt_NT(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(Alt_SPAN))
            in
              ((Alt_RES), FULL_SPAN, strm')
            end
      fun Definition_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Definition_PROD_1_SUBRULE_1_PRED, Definition_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(IDENT_SPAN), #2(SR_SPAN))
      in
        (UserCode.Definition_PROD_1_ACT (SR_RES, COLONCOLONEQ_RES, IDENT_RES, Alt_RES, SR_SPAN : (Lex.pos * Lex.pos), COLONCOLONEQ_SPAN : (Lex.pos * Lex.pos), IDENT_SPAN : (Lex.pos * Lex.pos), Alt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Grammar_NT (strm) = let
      fun Grammar_PROD_1_SUBRULE_1_NT (strm) = let
            val (Definition_RES, Definition_SPAN, strm') = Definition_NT(strm)
            val FULL_SPAN = (#1(Definition_SPAN), #2(Definition_SPAN))
            in
              ((Definition_RES), FULL_SPAN, strm')
            end
      fun Grammar_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.IDENT(_), _, strm') => true
              | _ => false
            (* end case *))
      val (Definition_RES, Definition_SPAN, strm') = EBNF.closure(Grammar_PROD_1_SUBRULE_1_PRED, Grammar_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(Definition_SPAN), #2(Definition_SPAN))
      in
        (UserCode.Grammar_PROD_1_ACT (Definition_RES, Definition_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (Grammar_NT)
end
val Grammar_NT =  fn s => unwrap (Err.launch (eh, lexFn, Grammar_NT , true) s)

in (Grammar_NT) end
  in
fun parse lexFn  s = let val (Grammar_NT) = mk lexFn in Grammar_NT s end

  end

end
