
signature ARITH_AST = sig
  type 'a annot = { node : 'a , span : Annot.span }
  
  (* terminals *)
  type nat
  
  (* nonterminals *)
  datatype exp' = ExpNumber of number
    | ExpPlus of exp * exp
    | ExpTimes of exp * exp
    | ExpGroup of exp
  and number' = NumberNat of nat annot
  and repl' = ReplExp of exp
  withtype exp = exp' annot
  and number = number' annot
  and repl = repl' annot
  
end

functor ArithParser (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Nat : TERMINAL
  end
) :>
sig
  include ARITH_AST
  where type nat = Terminals.Nat.t
  
  type 'a parser
  
  datatype terminal_token = TerminalNat of nat
  
  structure TokenStream : sig
    type t
    datatype token =
      Keyword of String.string * Annot.span
    | Terminal of terminal_token * Annot.span
    
    datatype front = Nil | Cons of token * t
    val front : t -> front
    
    val pos : t -> Annot.pos
  end
  
  exception LexError of LexStream.stream
  val lex : Char.char Stream.stream -> Annot.pos -> TokenStream.t
  
  val parseExp : exp parser
  val parseNumber : number parser
  val parseRepl : repl parser
  
  datatype 'a result =
    Success of ('a * TokenStream.t) list
  | Fail of ParseError.t
  val parse : 'a parser -> TokenStream.t -> 'a result
  
end =
struct

  type 'a annot = { node : 'a , span : Annot.span }
  type nat = Terminals.Nat.t
  
  datatype exp' = ExpNumber of number
    | ExpPlus of exp * exp
    | ExpTimes of exp * exp
    | ExpGroup of exp
  and number' = NumberNat of nat annot
  and repl' = ReplExp of exp
  withtype exp = exp' annot
  and number = number' annot
  and repl = repl' annot
  
  datatype terminal_token = TerminalNat of Terminals.Nat.t
  
  structure Internal = ParseInternal (
    structure Trivial = Trivial
    structure Terminal = struct
      type t = terminal_token
      val name = fn v => (case v of
        TerminalNat _ => "nat"
      )
      val lex =
        [ (fn ts =>
            case Terminals.Nat.lex ts of
              SOME (v , ts') => SOME (TerminalNat v , ts')
            | NONE => NONE)
        ]
    end
    val keywords =
      [ ("(" , 0)
      , (")" , 1)
      , ("*" , 2)
      , ("+" , 3)
      ]
  )
  open Internal
  
  val parseTerminalNat = parseTerminal (fn TerminalNat v => SOME v) "nat"
    val parseExpDummy : exp t_dummy = dummy ()
    val parseNumberDummy : number t_dummy = dummy ()
    val parseReplDummy : repl t_dummy = dummy ()
  
  val lex = lex
  
    (* Exp *)
    val parseExp =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseNumber =
            create ExpNumber "Exp" "Number" (
              bind (parseNonterminal (deref parseNumberDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseGroup =
            create ExpGroup "Exp" "Group" (
              bind (keyword 0) (fn v0 =>
              bind (parseNonterminal (deref parseExpDummy)) (fn v1 =>
              bind (keyword 1) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseNumber
        , parseGroup
        ]
        end)
  
        val parseLevel40 = fix (fn parseLevel40 =>
        let
          val parseTimes =
            create ExpTimes "Exp" "Times" (
              bind (parseNonterminal parseLevel40) (fn v0 =>
              bind (keyword 2) (fn v1 =>
              bind (parseNonterminal (forget parseAtom)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseAtom)
        , parseTimes
        ]
        end)
  
        val parseLevel30 = fix (fn parseLevel30 =>
        let
          val parsePlus =
            create ExpPlus "Exp" "Plus" (
              bind (parseNonterminal parseLevel30) (fn v0 =>
              bind (keyword 3) (fn v1 =>
              bind (parseNonterminal (forget parseLevel40)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ (forget parseLevel40)
        , parsePlus
        ]
        end)
  
      in
        longest (forget parseLevel30)
      end
  
    (* Number *)
    val parseNumber =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseNat =
            create NumberNat "Number" "Nat" (
              bind (parseTerminalNat) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseNat
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
    (* Repl *)
    val parseRepl =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseExp =
            create ReplExp "Repl" "Exp" (
              bind (parseNonterminal (deref parseExpDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseExp
        ]
        end)
  
      in
        longest (forget parseAtom)
      end
  
  
  val () = set parseExpDummy parseExp
  val () = set parseNumberDummy parseNumber
  val () = set parseReplDummy parseRepl
  
  val parse = parse

end

functor ArithPrint (
  structure Ast : ARITH_AST
  structure Terminals : sig
    structure Nat : PRINT_TERMINAL where type t = Ast.nat
  end
) :>
sig
  
  val printNat : Ast.nat Ast.annot -> string
  val printExp : Ast.exp -> string
  val printNumber : Ast.number -> string
  val printRepl : Ast.repl -> string
  
  val prettyPrintNat : Ast.nat Ast.annot -> string
  val prettyPrintExp : Ast.exp -> string
  val prettyPrintNumber : Ast.number -> string
  val prettyPrintRepl : Ast.repl -> string
  
end = struct

  open Ast
  
  val push = PrintBuffer.push
  
  fun printNat buf
    ( { node , span = { start = { lineno , ... } , ... } } : nat annot ) =
    push buf (Terminals.Nat.show node) lineno
  
  fun printExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp ) =
    case node of
        ExpNumber v0 =>
          ( push buf "ExpNumber" lineno
          ; push buf "(" lineno
          ; printNumber buf v0
          ; push buf ")" lineno
          )
      | ExpPlus (v0 , v1) =>
          ( push buf "ExpPlus" lineno
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
      | ExpGroup v0 =>
          ( push buf "ExpGroup" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf ")" lineno
          )
  
  and printNumber buf
    ( { node , span = { start = { lineno , ... } , ... } } : number ) =
    case node of
        NumberNat v0 =>
          ( push buf "NumberNat" lineno
          ; push buf "(" lineno
          ; printNat buf v0
          ; push buf ")" lineno
          )
  
  and printRepl buf
    ( { node , span = { start = { lineno , ... } , ... } } : repl ) =
    case node of
        ReplExp v0 =>
          ( push buf "ReplExp" lineno
          ; push buf "(" lineno
          ; printExp buf v0
          ; push buf ")" lineno
          )
  
  
  fun print f = fn v =>
  let val buf = PrintBuffer.empty ()
  in f buf v
  ; PrintBuffer.toString buf
  end
  val printNat = print printNat
  val printExp = print printExp
  val printNumber = print printNumber
  val printRepl = print printRepl
  
  
  fun prettyPrintNat buf
    ( { node , span = { start = { lineno , ... } , ... } } : nat annot ) =
    push buf (Terminals.Nat.show node) lineno
  
  fun prettyPrintExp buf
    ( { node , span = { start = { lineno , ... } , ... } } : exp ) =
    let val prettyPrintSelf = prettyPrintExp
    in
    case node of
        ExpNumber v0 =>
          ( prettyPrintNumber buf v0)
      | ExpGroup v1 =>
          ( push buf "(" lineno
          ; prettyPrintExp buf v1
          ; push buf ")" lineno)
      | ExpTimes (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "*" lineno
          ; prettyPrintSelf buf v2)
      | ExpPlus (v0 , v2) =>
          ( prettyPrintSelf buf v0
          ; push buf "+" lineno
          ; prettyPrintSelf buf v2)
    end
  
  and prettyPrintNumber buf
    ( { node , span = { start = { lineno , ... } , ... } } : number ) =
    let val prettyPrintSelf = prettyPrintNumber
    in
    case node of
        NumberNat v0 =>
          ( prettyPrintNat buf v0)
    end
  
  and prettyPrintRepl buf
    ( { node , span = { start = { lineno , ... } , ... } } : repl ) =
    let val prettyPrintSelf = prettyPrintRepl
    in
    case node of
        ReplExp v0 =>
          ( prettyPrintExp buf v0)
    end
  
  
  val prettyPrintNat = print prettyPrintNat
  val prettyPrintExp = print prettyPrintExp
  val prettyPrintNumber = print prettyPrintNumber
  val prettyPrintRepl = print prettyPrintRepl
  
end

functor ArithRepl (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Nat : REPL_TERMINAL
  end
) :> sig
  val run : unit -> unit
end = struct

  structure Parser = ArithParser (
    structure Trivial = Trivial
    structure Terminals = Terminals
  )
  
  structure Print = ArithPrint (
    structure Ast = Parser
    structure Terminals = Terminals
  )
  
  structure Repl = Repl (
    structure Result = struct
      open Parser
      type t = repl
      val parse = parse parseRepl
      val print = Print.printRepl
    end
  )
  
  val run = Repl.run
end