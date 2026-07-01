structure UngramTokens =
  struct
    datatype token
      = ColonColonEq
      | Bar
      | Star
      | Plus
      | Query
      | LParen
      | RParen
      | LBracket
      | RBracket
      | Comma
      | Underscore
      | AssocLeft
      | AssocRight
      | AssocNone
      | RuleName
      | Assoc
      | Prec
      | StringLiteral of string
      | Terminal of string
      | Int of int
      | Ident of string
      | EOF
    val allToks = [
            ColonColonEq, Bar, Star, Plus, Query, LParen, RParen, LBracket, RBracket, Comma, Underscore, AssocLeft, AssocRight, AssocNone, RuleName, Assoc, Prec, EOF
           ]
    fun toString tok =
(case (tok)
 of (ColonColonEq) => "::="
  | (Bar) => "|"
  | (Star) => "*"
  | (Plus) => "+"
  | (Query) => "?"
  | (LParen) => "("
  | (RParen) => ")"
  | (LBracket) => "["
  | (RBracket) => "]"
  | (Comma) => ","
  | (Underscore) => "_"
  | (AssocLeft) => "left"
  | (AssocRight) => "right"
  | (AssocNone) => "none"
  | (RuleName) => "name"
  | (Assoc) => "assoc"
  | (Prec) => "prec"
  | (StringLiteral(_)) => "StringLiteral"
  | (Terminal(_)) => "Terminal"
  | (Int(_)) => "Int"
  | (Ident(_)) => "Ident"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (ColonColonEq) => false
  | (Bar) => false
  | (Star) => false
  | (Plus) => false
  | (Query) => false
  | (LParen) => false
  | (RParen) => false
  | (LBracket) => false
  | (RBracket) => false
  | (Comma) => false
  | (Underscore) => false
  | (AssocLeft) => false
  | (AssocRight) => false
  | (AssocNone) => false
  | (RuleName) => false
  | (Assoc) => false
  | (Prec) => false
  | (StringLiteral(_)) => false
  | (Terminal(_)) => false
  | (Int(_)) => false
  | (Ident(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* UngramTokens *)

functor UngramParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
UngramTokens
    structure UserCode =
      struct

  structure A = Ast

fun Grammar_PROD_1_ACT (Definition, Definition_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({ definitions = Definition } : A.grammar)
fun Definition_PROD_1_ACT (SR, ColonColonEq, Bar, Ident, Rule, SR_SPAN : (Lex.pos * Lex.pos), ColonColonEq_SPAN : (Lex.pos * Lex.pos), Bar_SPAN : (Lex.pos * Lex.pos), Ident_SPAN : (Lex.pos * Lex.pos), Rule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({ name = Ident , rule = Rule :: SR } : A.definition)
fun Properties_PROD_1_ACT (SR, Property, RBracket, Comma1, Comma2, LBracket, SR_SPAN : (Lex.pos * Lex.pos), Property_SPAN : (Lex.pos * Lex.pos), RBracket_SPAN : (Lex.pos * Lex.pos), Comma1_SPAN : (Lex.pos * Lex.pos), Comma2_SPAN : (Lex.pos * Lex.pos), LBracket_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Property :: SR)
fun Rule_PROD_1_ACT (SeqRule, Properties, SeqRule_SPAN : (Lex.pos * Lex.pos), Properties_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SeqRule , Properties)
fun Rule_PROD_2_ACT (Properties, Properties_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Seq nil , Properties)
fun SeqRule_PROD_1_ACT (PostfixRule, PostfixRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case PostfixRule
                       of [single] => single
                        | items => A.Seq items)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_1_ACT (AtomRule, Star, AtomRule_SPAN : (Lex.pos * Lex.pos), Star_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Star AtomRule)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_2_ACT (AtomRule, Plus, AtomRule_SPAN : (Lex.pos * Lex.pos), Plus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Plus AtomRule)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_3_ACT (AtomRule, Query, AtomRule_SPAN : (Lex.pos * Lex.pos), Query_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Opt AtomRule)
fun PostfixRule_PROD_1_SUBRULE_1_PROD_4_ACT (AtomRule, AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomRule)
fun PostfixRule_PROD_1_ACT (SR, AtomRule, SR_SPAN : (Lex.pos * Lex.pos), AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SR)
fun AtomRule_PROD_1_ACT (Ident, Ident_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Nonterminal Ident)
fun AtomRule_PROD_2_ACT (StringLiteral, StringLiteral_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Keyword StringLiteral)
fun AtomRule_PROD_3_ACT (Terminal, Terminal_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Terminal Terminal)
fun AtomRule_PROD_4_ACT (SeqRule, LParen, RParen, SeqRule_SPAN : (Lex.pos * Lex.pos), LParen_SPAN : (Lex.pos * Lex.pos), RParen_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SeqRule)
fun Property_PROD_1_ACT (Assoc, AssocKind, Assoc_SPAN : (Lex.pos * Lex.pos), AssocKind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Assoc AssocKind)
fun Property_PROD_2_ACT (Prec, Int, Prec_SPAN : (Lex.pos * Lex.pos), Int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Prec Int)
fun Property_PROD_3_ACT (RuleName, Ident, RuleName_SPAN : (Lex.pos * Lex.pos), Ident_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.RuleName Ident)
fun Property_PROD_4_ACT (RuleName, Underscore, RuleName_SPAN : (Lex.pos * Lex.pos), Underscore_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.RuleName "")
fun AssocKind_PROD_1_ACT (AssocLeft, AssocLeft_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Left)
fun AssocKind_PROD_2_ACT (AssocRight, AssocRight_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (A.Right)
fun AssocKind_PROD_3_ACT (AssocNone, AssocNone_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
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
fun matchColonColonEq strm = (case (lex(strm))
 of (Tok.ColonColonEq, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBar strm = (case (lex(strm))
 of (Tok.Bar, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchStar strm = (case (lex(strm))
 of (Tok.Star, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPlus strm = (case (lex(strm))
 of (Tok.Plus, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchQuery strm = (case (lex(strm))
 of (Tok.Query, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLParen strm = (case (lex(strm))
 of (Tok.LParen, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRParen strm = (case (lex(strm))
 of (Tok.RParen, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLBracket strm = (case (lex(strm))
 of (Tok.LBracket, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRBracket strm = (case (lex(strm))
 of (Tok.RBracket, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchComma strm = (case (lex(strm))
 of (Tok.Comma, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchUnderscore strm = (case (lex(strm))
 of (Tok.Underscore, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAssocLeft strm = (case (lex(strm))
 of (Tok.AssocLeft, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAssocRight strm = (case (lex(strm))
 of (Tok.AssocRight, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAssocNone strm = (case (lex(strm))
 of (Tok.AssocNone, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRuleName strm = (case (lex(strm))
 of (Tok.RuleName, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAssoc strm = (case (lex(strm))
 of (Tok.Assoc, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPrec strm = (case (lex(strm))
 of (Tok.Prec, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchStringLiteral strm = (case (lex(strm))
 of (Tok.StringLiteral(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchTerminal strm = (case (lex(strm))
 of (Tok.Terminal(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchInt strm = (case (lex(strm))
 of (Tok.Int(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchIdent strm = (case (lex(strm))
 of (Tok.Ident(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Grammar_NT) = 
let
fun AssocKind_NT (strm) = let
      fun AssocKind_PROD_1 (strm) = let
            val (AssocLeft_RES, AssocLeft_SPAN, strm') = matchAssocLeft(strm)
            val FULL_SPAN = (#1(AssocLeft_SPAN), #2(AssocLeft_SPAN))
            in
              (UserCode.AssocKind_PROD_1_ACT (AssocLeft_RES, AssocLeft_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AssocKind_PROD_2 (strm) = let
            val (AssocRight_RES, AssocRight_SPAN, strm') = matchAssocRight(strm)
            val FULL_SPAN = (#1(AssocRight_SPAN), #2(AssocRight_SPAN))
            in
              (UserCode.AssocKind_PROD_2_ACT (AssocRight_RES, AssocRight_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AssocKind_PROD_3 (strm) = let
            val (AssocNone_RES, AssocNone_SPAN, strm') = matchAssocNone(strm)
            val FULL_SPAN = (#1(AssocNone_SPAN), #2(AssocNone_SPAN))
            in
              (UserCode.AssocKind_PROD_3_ACT (AssocNone_RES, AssocNone_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.AssocNone, _, strm') => AssocKind_PROD_3(strm)
          | (Tok.AssocLeft, _, strm') => AssocKind_PROD_1(strm)
          | (Tok.AssocRight, _, strm') => AssocKind_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun Property_NT (strm) = let
      fun Property_PROD_1 (strm) = let
            val (Assoc_RES, Assoc_SPAN, strm') = matchAssoc(strm)
            val (AssocKind_RES, AssocKind_SPAN, strm') = AssocKind_NT(strm')
            val FULL_SPAN = (#1(Assoc_SPAN), #2(AssocKind_SPAN))
            in
              (UserCode.Property_PROD_1_ACT (Assoc_RES, AssocKind_RES, Assoc_SPAN : (Lex.pos * Lex.pos), AssocKind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Property_PROD_2 (strm) = let
            val (Prec_RES, Prec_SPAN, strm') = matchPrec(strm)
            val (Int_RES, Int_SPAN, strm') = matchInt(strm')
            val FULL_SPAN = (#1(Prec_SPAN), #2(Int_SPAN))
            in
              (UserCode.Property_PROD_2_ACT (Prec_RES, Int_RES, Prec_SPAN : (Lex.pos * Lex.pos), Int_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Property_PROD_3 (strm) = let
            val (RuleName_RES, RuleName_SPAN, strm') = matchRuleName(strm)
            val (Ident_RES, Ident_SPAN, strm') = matchIdent(strm')
            val FULL_SPAN = (#1(RuleName_SPAN), #2(Ident_SPAN))
            in
              (UserCode.Property_PROD_3_ACT (RuleName_RES, Ident_RES, RuleName_SPAN : (Lex.pos * Lex.pos), Ident_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Property_PROD_4 (strm) = let
            val (RuleName_RES, RuleName_SPAN, strm') = matchRuleName(strm)
            val (Underscore_RES, Underscore_SPAN, strm') = matchUnderscore(strm')
            val FULL_SPAN = (#1(RuleName_SPAN), #2(Underscore_SPAN))
            in
              (UserCode.Property_PROD_4_ACT (RuleName_RES, Underscore_RES, RuleName_SPAN : (Lex.pos * Lex.pos), Underscore_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.Prec, _, strm') => Property_PROD_2(strm)
          | (Tok.Assoc, _, strm') => Property_PROD_1(strm)
          | (Tok.RuleName, _, strm') =>
              (case (lex(strm'))
               of (Tok.Ident(_), _, strm') => Property_PROD_3(strm)
                | (Tok.Underscore, _, strm') => Property_PROD_4(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun Properties_NT (strm) = let
      val (LBracket_RES, LBracket_SPAN, strm') = matchLBracket(strm)
      fun Properties_PROD_1_SUBRULE_1_NT (strm) = let
            val (Comma_RES, Comma_SPAN, strm') = matchComma(strm)
            val FULL_SPAN = (#1(Comma_SPAN), #2(Comma_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun Properties_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Comma, _, strm') => true
              | _ => false
            (* end case *))
      val (Comma1_RES, Comma1_SPAN, strm') = EBNF.optional(Properties_PROD_1_SUBRULE_1_PRED, Properties_PROD_1_SUBRULE_1_NT, strm')
      val (Property_RES, Property_SPAN, strm') = Property_NT(strm')
      fun Properties_PROD_1_SUBRULE_2_NT (strm) = let
            val (Comma_RES, Comma_SPAN, strm') = matchComma(strm)
            val (Property_RES, Property_SPAN, strm') = Property_NT(strm')
            val FULL_SPAN = (#1(Comma_SPAN), #2(Property_SPAN))
            in
              ((Property_RES), FULL_SPAN, strm')
            end
      fun Properties_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.Comma, _, strm') =>
                  (case (lex(strm'))
                   of (Tok.RuleName, _, strm') => true
                    | (Tok.Assoc, _, strm') => true
                    | (Tok.Prec, _, strm') => true
                    | _ => false
                  (* end case *))
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Properties_PROD_1_SUBRULE_2_PRED, Properties_PROD_1_SUBRULE_2_NT, strm')
      fun Properties_PROD_1_SUBRULE_3_NT (strm) = let
            val (Comma_RES, Comma_SPAN, strm') = matchComma(strm)
            val FULL_SPAN = (#1(Comma_SPAN), #2(Comma_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun Properties_PROD_1_SUBRULE_3_PRED (strm) = (case (lex(strm))
             of (Tok.Comma, _, strm') => true
              | _ => false
            (* end case *))
      val (Comma2_RES, Comma2_SPAN, strm') = EBNF.optional(Properties_PROD_1_SUBRULE_3_PRED, Properties_PROD_1_SUBRULE_3_NT, strm')
      val (RBracket_RES, RBracket_SPAN, strm') = matchRBracket(strm')
      val FULL_SPAN = (#1(LBracket_SPAN), #2(RBracket_SPAN))
      in
        (UserCode.Properties_PROD_1_ACT (SR_RES, Property_RES, RBracket_RES, Comma1_RES, Comma2_RES, LBracket_RES, SR_SPAN : (Lex.pos * Lex.pos), Property_SPAN : (Lex.pos * Lex.pos), RBracket_SPAN : (Lex.pos * Lex.pos), Comma1_SPAN : (Lex.pos * Lex.pos), Comma2_SPAN : (Lex.pos * Lex.pos), LBracket_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun SeqRule_NT (strm) = let
      fun SeqRule_PROD_1_SUBRULE_1_NT (strm) = let
            val (PostfixRule_RES, PostfixRule_SPAN, strm') = PostfixRule_NT(strm)
            val FULL_SPAN = (#1(PostfixRule_SPAN), #2(PostfixRule_SPAN))
            in
              ((PostfixRule_RES), FULL_SPAN, strm')
            end
      fun SeqRule_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LParen, _, strm') => true
              | (Tok.StringLiteral(_), _, strm') => true
              | (Tok.Terminal(_), _, strm') => true
              | (Tok.Ident(_), _, strm') => true
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
                  val (Star_RES, Star_SPAN, strm') = matchStar(strm)
                  val FULL_SPAN = (#1(Star_SPAN), #2(Star_SPAN))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_1_ACT (AtomRule_RES, Star_RES, AtomRule_SPAN : (Lex.pos * Lex.pos), Star_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun PostfixRule_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (Plus_RES, Plus_SPAN, strm') = matchPlus(strm)
                  val FULL_SPAN = (#1(Plus_SPAN), #2(Plus_SPAN))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_2_ACT (AtomRule_RES, Plus_RES, AtomRule_SPAN : (Lex.pos * Lex.pos), Plus_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun PostfixRule_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                  val (Query_RES, Query_SPAN, strm') = matchQuery(strm)
                  val FULL_SPAN = (#1(Query_SPAN), #2(Query_SPAN))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_3_ACT (AtomRule_RES, Query_RES, AtomRule_SPAN : (Lex.pos * Lex.pos), Query_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun PostfixRule_PROD_1_SUBRULE_1_PROD_4 (strm) = let
                  val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
                  in
                    (UserCode.PostfixRule_PROD_1_SUBRULE_1_PROD_4_ACT (AtomRule_RES, AtomRule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm)
                  end
            in
              (case (lex(strm))
               of (Tok.LParen, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.RParen, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.LBracket, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.StringLiteral(_), _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.Terminal(_), _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.Ident(_), _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_4(strm)
                | (Tok.Plus, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.Star, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_1(strm)
                | (Tok.Query, _, strm') =>
                    PostfixRule_PROD_1_SUBRULE_1_PROD_3(strm)
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
            val (Ident_RES, Ident_SPAN, strm') = matchIdent(strm)
            val FULL_SPAN = (#1(Ident_SPAN), #2(Ident_SPAN))
            in
              (UserCode.AtomRule_PROD_1_ACT (Ident_RES, Ident_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomRule_PROD_2 (strm) = let
            val (StringLiteral_RES, StringLiteral_SPAN, strm') = matchStringLiteral(strm)
            val FULL_SPAN = (#1(StringLiteral_SPAN), #2(StringLiteral_SPAN))
            in
              (UserCode.AtomRule_PROD_2_ACT (StringLiteral_RES, StringLiteral_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomRule_PROD_3 (strm) = let
            val (Terminal_RES, Terminal_SPAN, strm') = matchTerminal(strm)
            val FULL_SPAN = (#1(Terminal_SPAN), #2(Terminal_SPAN))
            in
              (UserCode.AtomRule_PROD_3_ACT (Terminal_RES, Terminal_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomRule_PROD_4 (strm) = let
            val (LParen_RES, LParen_SPAN, strm') = matchLParen(strm)
            val (SeqRule_RES, SeqRule_SPAN, strm') = SeqRule_NT(strm')
            val (RParen_RES, RParen_SPAN, strm') = matchRParen(strm')
            val FULL_SPAN = (#1(LParen_SPAN), #2(RParen_SPAN))
            in
              (UserCode.AtomRule_PROD_4_ACT (SeqRule_RES, LParen_RES, RParen_RES, SeqRule_SPAN : (Lex.pos * Lex.pos), LParen_SPAN : (Lex.pos * Lex.pos), RParen_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LParen, _, strm') => AtomRule_PROD_4(strm)
          | (Tok.StringLiteral(_), _, strm') => AtomRule_PROD_2(strm)
          | (Tok.Ident(_), _, strm') => AtomRule_PROD_1(strm)
          | (Tok.Terminal(_), _, strm') => AtomRule_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun Rule_NT (strm) = let
      fun Rule_PROD_1 (strm) = let
            val (SeqRule_RES, SeqRule_SPAN, strm') = SeqRule_NT(strm)
            val (Properties_RES, Properties_SPAN, strm') = Properties_NT(strm')
            val FULL_SPAN = (#1(SeqRule_SPAN), #2(Properties_SPAN))
            in
              (UserCode.Rule_PROD_1_ACT (SeqRule_RES, Properties_RES, SeqRule_SPAN : (Lex.pos * Lex.pos), Properties_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Rule_PROD_2 (strm) = let
            val (Properties_RES, Properties_SPAN, strm') = Properties_NT(strm)
            val FULL_SPAN = (#1(Properties_SPAN), #2(Properties_SPAN))
            in
              (UserCode.Rule_PROD_2_ACT (Properties_RES, Properties_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LBracket, _, strm') => Rule_PROD_2(strm)
          | (Tok.LParen, _, strm') => Rule_PROD_1(strm)
          | (Tok.StringLiteral(_), _, strm') => Rule_PROD_1(strm)
          | (Tok.Terminal(_), _, strm') => Rule_PROD_1(strm)
          | (Tok.Ident(_), _, strm') => Rule_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Definition_NT (strm) = let
      val (Ident_RES, Ident_SPAN, strm') = matchIdent(strm)
      val (ColonColonEq_RES, ColonColonEq_SPAN, strm') = matchColonColonEq(strm')
      fun Definition_PROD_1_SUBRULE_1_NT (strm) = let
            val (Bar_RES, Bar_SPAN, strm') = matchBar(strm)
            val FULL_SPAN = (#1(Bar_SPAN), #2(Bar_SPAN))
            in
              ((), FULL_SPAN, strm')
            end
      fun Definition_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.Bar, _, strm') => true
              | _ => false
            (* end case *))
      val (Bar_RES, Bar_SPAN, strm') = EBNF.optional(Definition_PROD_1_SUBRULE_1_PRED, Definition_PROD_1_SUBRULE_1_NT, strm')
      val (Rule_RES, Rule_SPAN, strm') = Rule_NT(strm')
      fun Definition_PROD_1_SUBRULE_2_NT (strm) = let
            val (Bar_RES, Bar_SPAN, strm') = matchBar(strm)
            val (Rule_RES, Rule_SPAN, strm') = Rule_NT(strm')
            val FULL_SPAN = (#1(Bar_SPAN), #2(Rule_SPAN))
            in
              ((Rule_RES), FULL_SPAN, strm')
            end
      fun Definition_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.Bar, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Definition_PROD_1_SUBRULE_2_PRED, Definition_PROD_1_SUBRULE_2_NT, strm')
      val FULL_SPAN = (#1(Ident_SPAN), #2(SR_SPAN))
      in
        (UserCode.Definition_PROD_1_ACT (SR_RES, ColonColonEq_RES, Bar_RES, Ident_RES, Rule_RES, SR_SPAN : (Lex.pos * Lex.pos), ColonColonEq_SPAN : (Lex.pos * Lex.pos), Bar_SPAN : (Lex.pos * Lex.pos), Ident_SPAN : (Lex.pos * Lex.pos), Rule_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
             of (Tok.Ident(_), _, strm') => true
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
