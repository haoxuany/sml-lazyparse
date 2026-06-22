functor Calc (
  val table_size : int
  structure Stream : STREAM
  structure Trivial : TERMINAL where type 'a stream = 'a Stream.stream
  structure Terminals : sig
    structure Number : TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream
  end
) :>

sig
  type 'a annot = { node : 'a , span : Annot.span }
  datatype stmt' = StmtExp of exp
  | StmtIfThenElse of exp * stmt * stmt
  and exp' = ExpPlus of exp * exp
  | ExpMinus of exp * exp
  | ExpTimes of exp * exp
  | ExpDiv of exp * exp
  | ExpParens of exp
  | ExpNum of Terminals.Number.t annot
  withtype stmt = stmt' annot
  and exp = exp' annot
  
  val printStmt : stmt -> string
  val printExp : exp -> string
  
  type 'a parser
  type token_stream
  val lex : char Stream.stream -> Annot.pos -> token_stream
  val parseStmt : stmt parser
  val parseExp : exp parser
  val parse : 'a parser -> token_stream -> ('a * token_stream) list
end =
struct

  type 'a annot = { node : 'a , span : Annot.span }
  
  datatype stmt' = StmtExp of exp
    | StmtIfThenElse of exp * stmt * stmt
  and exp' = ExpPlus of exp * exp
    | ExpMinus of exp * exp
    | ExpTimes of exp * exp
    | ExpDiv of exp * exp
    | ExpParens of exp
    | ExpNum of Terminals.Number.t annot
  withtype stmt = stmt' annot
  and exp = exp' annot
  
  local
    structure LexInternal = LexInternal (structure Stream = Stream)
    datatype terminal_token = TerminalNumber of Terminals.Number.t
    val keywords =
      [ ("(" , 0)
      , (")" , 1)
      , ("*" , 2)
      , ("+" , 3)
      , ("-" , 4)
      , ("/" , 5)
      , ("else" , 6)
      , ("if" , 7)
      , ("then" , 8)
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
    val parseTerminalNumber = parseTerminal (fn TerminalNumber v => SOME v)
    val parseStmtDummy : stmt t_dummy = dummy ()
    val parseExpDummy : exp t_dummy = dummy ()
  
  in
  type 'a parser = 'a t_memo
  type token_stream = (int , Trivial.t , terminal_token) LexInternal.token Stream.stream
  
  fun lex s pos = LexInternal.lex s pos keywords Trivial.lex
    [ fn x => case Terminals.Number.lex x of SOME (v , s , p) => SOME (TerminalNumber v , s , p) | NONE => NONE
    ]
  
    (* Stmt *)
    val parseStmt =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseExp =
            bind (deref parseExpDummy) (fn v0 as { span = v0_span , ... } =>
            return
              { node = StmtExp v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              })
  
        in either
        [ parseExp
        ]
        end)
  
        val parseLevel5 = fix (fn parseLevel5 =>
        let
          val parseIfThenElse =
            bind skipTrivial (fn _ =>
            bind (keyword 7) (fn v0 =>
            bind (deref parseExpDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 8) (fn v2 =>
            bind (forget parseAtom) (fn v3 as { span = v3_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 6) (fn v4 =>
            bind parseLevel5 (fn v5 as { span = v5_span , ... } =>
            return
              { node = StmtIfThenElse (v1 , v3 , v5)
              , span = Annot.span (#start v0) (#finish v5_span)
              })))))))))
  
        in either
        [ (forget parseAtom)
        , parseIfThenElse
        ]
        end)
  
      in
        forget parseLevel5
      end
  
    (* Exp *)
    val parseExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseParens =
            bind skipTrivial (fn _ =>
            bind (keyword 0) (fn v0 =>
            bind (deref parseExpDummy) (fn v1 as { span = v1_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 1) (fn v2 =>
            return
              { node = ExpParens v1
              , span = Annot.span (#start v0) (#finish v2)
              })))))
  
          val parseNum =
            bind skipTrivial (fn _ =>
            bind (parseTerminalNumber) (fn v0 as { span = v0_span , ... } =>
            return
              { node = ExpNum v0
              , span = Annot.span (#start v0_span) (#finish v0_span)
              }))
  
        in either
        [ parseParens
        , parseNum
        ]
        end)
  
        val parseLevel2 = fix (fn parseLevel2 =>
        let
          val parseTimes =
            bind parseLevel2 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 2) (fn v1 =>
            bind (forget parseAtom) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpTimes (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
          val parseDiv =
            bind parseLevel2 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 5) (fn v1 =>
            bind (forget parseAtom) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpDiv (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
        in either
        [ (forget parseAtom)
        , parseTimes
        , parseDiv
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parsePlus =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 3) (fn v1 =>
            bind (forget parseLevel2) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpPlus (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
          val parseMinus =
            bind parseLevel1 (fn v0 as { span = v0_span , ... } =>
            bind skipTrivial (fn _ =>
            bind (keyword 4) (fn v1 =>
            bind (forget parseLevel2) (fn v2 as { span = v2_span , ... } =>
            return
              { node = ExpMinus (v0 , v2)
              , span = Annot.span (#start v0_span) (#finish v2_span)
              }))))
  
        in either
        [ (forget parseLevel2)
        , parsePlus
        , parseMinus
        ]
        end)
  
      in
        forget parseLevel1
      end
  
  
  val () = set parseStmtDummy parseStmt
  val () = set parseExpDummy parseExp
  
    fun printStmt buf ({ node , span = _ } : stmt) =
      case node of
        StmtExp v0 =>
          printExp buf v0
        | StmtIfThenElse (v0 , v1 , v2) =>
          (
          PrintBuffer.push buf "if" 0;
          printExp buf v0;
          PrintBuffer.push buf "then" 0;
          printStmt buf v1;
          PrintBuffer.push buf "else" 0;
          printStmt buf v2
          )
    and printExp buf ({ node , span = _ } : exp) =
      case node of
        ExpPlus (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "+" 0;
          printExp buf v1
          )
        | ExpMinus (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "-" 0;
          printExp buf v1
          )
        | ExpTimes (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "*" 0;
          printExp buf v1
          )
        | ExpDiv (v0 , v1) =>
          (
          printExp buf v0;
          PrintBuffer.push buf "/" 0;
          printExp buf v1
          )
        | ExpParens v0 =>
          (
          PrintBuffer.push buf "(" 0;
          printExp buf v0;
          PrintBuffer.push buf ")" 0
          )
        | ExpNum v0 =>
          let val { node = v0_node , span = { start = { lineno = v0_line , ... } , ... } } = v0
          in PrintBuffer.push buf (Terminals.Number.show v0_node) v0_line end
  
  fun print f v = let val buf = PrintBuffer.empty () in f buf v; PrintBuffer.toString buf end
  val printStmt = print printStmt
  val printExp = print printExp
  
  val parse = parser
  
  end

end