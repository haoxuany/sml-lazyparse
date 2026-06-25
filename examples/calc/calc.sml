signature CALC_AST = sig
  type 'a annot = { node : 'a , span : Annot.span }
  
  (* terminals *)
  type number
  
  (* nonterminals *)
  datatype stmt' = StmtExp of exp
    | StmtIfThenElse of exp * (stmt * stmt)
  and exp' = ExpPlus of exp * exp
    | ExpMinus of exp * exp
    | ExpTimes of exp * exp
    | ExpDiv of exp * exp
    | ExpParens of exp
    | ExpNum of number annot
  withtype stmt = stmt' annot
  and exp = exp' annot
  
end

functor CalcParser (
  structure Stream : STREAM
  structure Trivial : TERMINAL where type 'a stream = 'a Stream.stream
  structure Terminals : sig
    structure Number : TERMINAL where type 'a stream = 'a Stream.stream
  end
) :>
sig
  include CALC_AST
  where type number = Terminals.Number.t
  
  type 'a parser
  type token_stream
  val lex : Char.char Stream.stream -> Annot.pos -> token_stream
  val parseStmt : stmt parser
  val parseExp : exp parser
  val parse : 'a parser -> token_stream -> ('a * token_stream) list
end =
struct

  type 'a annot = { node : 'a , span : Annot.span }
  type number = Terminals.Number.t
  
  datatype stmt' = StmtExp of exp
    | StmtIfThenElse of exp * (stmt * stmt)
  and exp' = ExpPlus of exp * exp
    | ExpMinus of exp * exp
    | ExpTimes of exp * exp
    | ExpDiv of exp * exp
    | ExpParens of exp
    | ExpNum of number annot
  withtype stmt = stmt' annot
  and exp = exp' annot
  
  datatype terminal_token = TerminalNumber of Terminals.Number.t
  
  structure Internal = ParseInternal (
    structure Stream = Stream
    structure Trivial = Trivial
    structure Terminal = struct
      type t = terminal_token
      type 'a stream = 'a Stream.stream
      val lex =
        [ (fn (s , p) =>
            case Terminals.Number.lex (s , p) of
              SOME (v , s' , p') => SOME (TerminalNumber v , s' , p')
            | NONE => NONE)
        ]
    end
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
  )
  open Internal
  
  val parseTerminalNumber = parseTerminal (fn TerminalNumber v => SOME v)
    val parseStmtDummy : stmt t_dummy = dummy ()
    val parseExpDummy : exp t_dummy = dummy ()
  
  val lex = lex
  
    (* Stmt *)
    val parseStmt =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseIfThenElse =
            create StmtIfThenElse (
              bind (keyword 7) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (
                bind (keyword 8) (fn v3 =>
                bind (parseNonterminal (deref parseStmtDummy)) (fn v4 =>
                bind (keyword 6) (fn v5 =>
                bind (parseNonterminal (deref parseStmtDummy)) (fn v6 =>
                return_node ((#node v4) , (#node v6)) [ annot_add v3 , annot_add v4 , annot_add v5 , annot_add v6 ])))))
              (fn v2 =>
              return_node ((#node v1) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseExp =
            create StmtExp (
              bind (parseNonterminal (deref parseExpDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseIfThenElse
        , parseExp
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
          val parseNum =
            create ExpNum (
              bind (parseTerminalNumber) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseParens =
            create ExpParens (
              bind (keyword 0) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 1) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseNum
        , parseParens
        ]
        end)
  
        val parseLevel2 = fix (fn parseLevel2 =>
        let
          val parseDiv =
            create ExpDiv (
              bind (parseNonterminal parseLevel2) (fn v0 =>
              bind (keyword 5) (fn v1 =>
              bind (parseNonterminal (forget parseAtom)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parseTimes =
            create ExpTimes (
              bind (parseNonterminal parseLevel2) (fn v0 =>
              bind (keyword 2) (fn v1 =>
              bind (parseNonterminal (forget parseAtom)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseAtom)
        , parseDiv
        , parseTimes
        ]
        end)
  
        val parseLevel1 = fix (fn parseLevel1 =>
        let
          val parseMinus =
            create ExpMinus (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 4) (fn v1 =>
              bind (parseNonterminal (forget parseLevel2)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
          val parsePlus =
            create ExpPlus (
              bind (parseNonterminal parseLevel1) (fn v0 =>
              bind (keyword 3) (fn v1 =>
              bind (parseNonterminal (forget parseLevel2)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseLevel2)
        , parseMinus
        , parsePlus
        ]
        end)
  
      in
        forget parseLevel1
      end
  
  
  val () = set parseStmtDummy parseStmt
  val () = set parseExpDummy parseExp
  
  val parse = parser

end

functor CalcPrint (
  structure Ast : CALC_AST
  structure Terminals : sig
    structure Number : PRINT_TERMINAL where type t = Ast.number
  end
) :>
sig
  val printNumber : Ast.number Ast.annot -> string
  val printStmt : Ast.stmt -> string
  val printExp : Ast.exp -> string
  val prettyPrintNumber : Ast.number Ast.annot -> string
  val prettyPrintStmt : Ast.stmt -> string
  val prettyPrintExp : Ast.exp -> string
  
end = struct

  open Ast
  
  val push = PrintBuffer.push
  
  fun printNumber buf
    ( { node , span = { start = { lineno , ... } , ... } } : number annot ) =
    push buf (Terminals.Number.show node) lineno
  
  fun printStmt buf
    ( { node , span = { start = { lineno , ... } , ... } } : stmt ) =
    case node of
        StmtExp v0 =>
          ( push buf "StmtExp" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf ")" lineno
          )
      | StmtIfThenElse (v0 , v1) =>
          ( push buf "StmtIfThenElse" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; let val (v10 , v11) = v1 in push buf "(" lineno ; printStmt buf v10 ; push buf " , " lineno ; printStmt buf v11 ; push buf ")" lineno end
          ; push buf ")" lineno
          )
  
  and printExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp ) =
    case node of
        ExpPlus (v0 , v1) =>
          ( push buf "ExpPlus" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpMinus (v0 , v1) =>
          ( push buf "ExpMinus" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpTimes (v0 , v1) =>
          ( push buf "ExpTimes" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpDiv (v0 , v1) =>
          ( push buf "ExpDiv" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf " , " lineno
          ; printExp buf v1
          ; push buf ")" lineno
          )
      | ExpParens v0 =>
          ( push buf "ExpParens" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf ")" lineno
          )
      | ExpNum v0 =>
          ( push buf "ExpNum" lineno
          ; push buf "(" lineno
          ; printNumber buf v0
          ; push buf ")" lineno
          )
  
  
  fun print f = fn v =>
  let val buf = PrintBuffer.empty ()
  in f buf v
  ; PrintBuffer.toString buf
  end
  val printNumber = print printNumber
  val printStmt = print printStmt
  val printExp = print printExp
  
  
  fun prettyPrintNumber buf
    ( { node , span = { start = { lineno , ... } , ... } } : number annot ) =
    push buf (Terminals.Number.show node) lineno
  
  fun prettyPrintStmt buf
    ( { node , span = { start = { lineno , ... } , ... } } : stmt ) =
    let val prettyPrintSelf = prettyPrintStmt
    in
    case node of
        StmtIfThenElse (v1 , v2) =>
          ( push buf "if" lineno
          ; prettyPrintExp buf v1
          ; let val (v4 , v6) = v2
              in push buf "then" lineno
              ; prettyPrintStmt buf v4
              ; push buf "else" lineno
              ; prettyPrintStmt buf v6
              end)
      | StmtExp v0 =>
          ( prettyPrintExp buf v0)
    end
  
  and prettyPrintExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp ) =
    let val prettyPrintSelf = prettyPrintExp
    in
    case node of
        ExpNum v0 =>
          ( prettyPrintNumber buf v0)
      | ExpParens v1 =>
          ( push buf "(" lineno
          ; prettyPrintExp buf v1
          ; push buf ")" lineno)
      | ExpDiv (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "/" lineno
          ; prettyPrintSelf buf v2)
      | ExpTimes (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "*" lineno
          ; prettyPrintSelf buf v2)
      | ExpMinus (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "-" lineno
          ; prettyPrintSelf buf v2)
      | ExpPlus (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "+" lineno
          ; prettyPrintSelf buf v2)
    end
  
  
  val prettyPrintNumber = print prettyPrintNumber
  val prettyPrintStmt = print prettyPrintStmt
  val prettyPrintExp = print prettyPrintExp
  
end