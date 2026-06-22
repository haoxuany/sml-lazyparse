structure UngramLex  = struct

    datatype yystart_state = 
SINGLEQUOTE | COM | DOUBLEQUOTE | INITIAL
    local

    structure UserDeclarations = 
      struct

 

structure Tok = UngramTokens

val commentLevel : int ref = ref 0

type lex_result = Tok.token

fun eof () = 
( if !commentLevel > 0
  then raise Fail "unterminated comment on EOF"
  else ();

  Tok.EOF
)

local
  val text : string list ref = ref []
in

fun pushText s = (text := s :: (!text))
val text = fn () =>
  let
    val result = String.concat (List.rev (!text))
  in
    text := [] ;
    result
  end

end

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  skip())
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
       commentLevel := 1; YYBEGIN COM; continue ())
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
       commentLevel := !commentLevel + 1; continue ())
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       commentLevel := !commentLevel - 1;
            if !commentLevel = 0
              then (YYBEGIN INITIAL; continue ())
              else continue ())
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  continue ())
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.ColonColonEq)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Bar)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Star)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Plus)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Query)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LParen)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RParen)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.LBracket)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RBracket)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Comma)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN DOUBLEQUOTE; continue())
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.StringLiteral (text()))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
       pushText "\""; continue())
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
       pushText "\\"; continue())
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
       pushText "\n"; continue())
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
       pushText "\t"; continue())
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.StringLiteral (text()))
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  pushText yytext; continue()
      end
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN SINGLEQUOTE; continue())
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.Terminal (text()))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
       pushText "'"; continue())
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
       pushText "\\"; continue())
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; Tok.Terminal (text()))
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  pushText yytext; continue()
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.Int (valOf (Int.fromString yytext))
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.AssocLeft)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.AssocRight)
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.AssocNone)
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.RuleName)
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Assoc)
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;  Tok.Prec)
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  Tok.Ident yytext
      end
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;  continue())
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction32(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction32(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction32(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction32(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ58(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ56(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ55(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction36(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction36(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction36(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction36(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction36(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ61(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx63
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ60(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ59(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction33(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction33(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction33(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ65(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ64(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction34(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction34(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction34(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction34(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ67(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ66(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction37(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else yyAction37(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ63(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx61
                  then yyQ62(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction31(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction31(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction31(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ70(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ69(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ68(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction35(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction35(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction35(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction35(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ74(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx63
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ73(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ72(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ71(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction37(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ76(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ75(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ77(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ77(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ77(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ77(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wxD
              then yyAction1(strm, yyNO_MATCH)
              else yyQ79(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2B
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx2B
              then if inp = 0wx2A
                  then yyQ78(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = 0wx2F
              then yyQ79(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ81(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
and yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ81(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ81(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wxE
              then if inp = 0wx9
                  then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx9
                  then yyAction0(strm, yyNO_MATCH)
                else if inp = 0wxD
                  then yyQ81(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx20
              then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3F
              then yyQ44(strm', lastMatch)
            else if inp < 0wx3F
              then if inp = 0wx28
                  then yyQ36(strm', lastMatch)
                else if inp < 0wx28
                  then if inp = 0wx20
                      then yyQ32(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wxD
                          then yyQ33(strm', lastMatch)
                        else if inp < 0wxD
                          then if inp <= 0wx8
                              then yyQ31(strm', lastMatch)
                              else yyQ32(strm', lastMatch)
                          else yyQ31(strm', lastMatch)
                    else if inp = 0wx23
                      then yyQ31(strm', lastMatch)
                    else if inp < 0wx23
                      then if inp = 0wx21
                          then yyQ31(strm', lastMatch)
                          else yyQ34(strm', lastMatch)
                    else if inp = 0wx27
                      then yyQ35(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wx2D
                  then yyQ31(strm', lastMatch)
                else if inp < 0wx2D
                  then if inp = 0wx2B
                      then yyQ39(strm', lastMatch)
                    else if inp < 0wx2B
                      then if inp = 0wx29
                          then yyQ37(strm', lastMatch)
                          else yyQ38(strm', lastMatch)
                      else yyQ40(strm', lastMatch)
                else if inp = 0wx30
                  then yyQ42(strm', lastMatch)
                else if inp < 0wx30
                  then if inp = 0wx2F
                      then yyQ41(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wx3A
                  then yyQ43(strm', lastMatch)
                else if inp <= 0wx39
                  then yyQ42(strm', lastMatch)
                  else yyQ31(strm', lastMatch)
            else if inp = 0wx6D
              then yyQ45(strm', lastMatch)
            else if inp < 0wx6D
              then if inp = 0wx5D
                  then yyQ47(strm', lastMatch)
                else if inp < 0wx5D
                  then if inp = 0wx5B
                      then yyQ46(strm', lastMatch)
                    else if inp < 0wx5B
                      then if inp = 0wx40
                          then yyQ31(strm', lastMatch)
                          else yyQ45(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wx62
                  then yyQ45(strm', lastMatch)
                else if inp < 0wx62
                  then if inp = 0wx61
                      then yyQ48(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wx6C
                  then yyQ49(strm', lastMatch)
                  else yyQ45(strm', lastMatch)
            else if inp = 0wx72
              then yyQ52(strm', lastMatch)
            else if inp < 0wx72
              then if inp = 0wx70
                  then yyQ51(strm', lastMatch)
                else if inp < 0wx70
                  then if inp = 0wx6E
                      then yyQ50(strm', lastMatch)
                      else yyQ45(strm', lastMatch)
                  else yyQ45(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ53(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx7B
                  then yyQ31(strm', lastMatch)
                  else yyQ45(strm', lastMatch)
              else yyQ31(strm', lastMatch)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx5D
              then if inp = 0wx23
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < 0wx23
                  then if inp = 0wx22
                      then yyQ25(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = 0wx5C
                  then yyQ26(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ27(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ28(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ29(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction23(strm, yyNO_MATCH)
              else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction23(strm, yyNO_MATCH)
                      else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx23
              then yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyAction23(strm, yyNO_MATCH)
                  else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction23(strm, yyNO_MATCH)
              else yyQ30(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ20(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ20(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ21(strm', lastMatch)
                      else yyQ20(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ22(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = 0wx23
              then yyQ20(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ23(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ24(strm', lastMatch)
              else yyQ20(strm', lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ17(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ18(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ19(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ15(strm', lastMatch)
            else if inp < 0wx2A
              then if inp = 0wxD
                  then yyQ14(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = 0wx2F
              then yyQ16(strm', lastMatch)
              else yyQ13(strm', lastMatch)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx28
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx28
              then if inp = 0wx27
                  then yyQ9(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = 0wx5C
              then yyQ10(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ11(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx28
              then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx28
              then if inp = 0wx27
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction29(strm, yyNO_MATCH)
              else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyAction29(strm, yyNO_MATCH)
                      else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = 0wxD
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx28
              then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < 0wx28
              then if inp = 0wx27
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = 0wx5C
              then yyAction29(strm, yyNO_MATCH)
              else yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ4(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ4(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ5(strm', lastMatch)
                      else yyQ4(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ6(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx28
              then yyQ4(strm', lastMatch)
            else if inp < 0wx28
              then if inp = 0wx27
                  then yyQ7(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ8(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of SINGLEQUOTE => yyQ0(!(yystrm), yyNO_MATCH)
    | COM => yyQ1(!(yystrm), yyNO_MATCH)
    | DOUBLEQUOTE => yyQ2(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()-1), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

