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
  | StmtIfThenElse of exp * (stmt * stmt)
  and exp' = ExpPlus of exp * exp
  | ExpMinus of exp * exp
  | ExpTimes of exp * exp
  | ExpDiv of exp * exp
  | ExpParens of exp
  | ExpNum of Terminals.Number.t annot
  withtype stmt = stmt' annot
  and exp = exp' annot
  
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
    | StmtIfThenElse of exp * (stmt * stmt)
  and exp' = ExpPlus of exp * exp
    | ExpMinus of exp * exp
    | ExpTimes of exp * exp
    | ExpDiv of exp * exp
    | ExpParens of exp
    | ExpNum of Terminals.Number.t annot
  withtype stmt = stmt' annot
  and exp = exp' annot
  
  datatype terminal_token = TerminalNumber of Terminals.Number.t
  
  structure Internal = ParseInternal (
    val table_size = table_size
    structure Stream = Stream
    structure Trivial = Trivial
    type terminal = terminal_token
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
    [ addLexer Terminals.Number.lex TerminalNumber
    ]
  
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