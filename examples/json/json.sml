signature JSON_AST = sig
  type 'a annot = { node : 'a , span : Annot.span }
  
  (* terminals *)
  type number
  type string
  
  (* nonterminals *)
  datatype value' = ValueString of string annot
    | ValueNumber of number annot
    | ValueObject of object
    | ValueArray of array
    | ValueTrue
    | ValueFalse
    | ValueNull
  and object' = ObjectObject of (member * member list) option
  and member' = MemberMember of string annot * value
  and array' = ArrayArray of (value * value list) option
  withtype value = value' annot
  and object = object' annot
  and member = member' annot
  and array = array' annot
  
end

functor JsonParser (
  structure Stream : STREAM
  structure Trivial : TERMINAL where type 'a stream = 'a Stream.stream
  structure Terminals : sig
    structure Number : TERMINAL where type 'a stream = 'a Stream.stream
    structure String : TERMINAL where type 'a stream = 'a Stream.stream
  end
) :>
sig
  include JSON_AST
  where type number = Terminals.Number.t
  where type string = Terminals.String.t
  
  type 'a parser
  type token_stream
  val lex : Char.char Stream.stream -> Annot.pos -> token_stream
  val parseValue : value parser
  val parseObject : object parser
  val parseMember : member parser
  val parseArray : array parser
  val parse : 'a parser -> token_stream -> ('a * token_stream) list
end =
struct

  type 'a annot = { node : 'a , span : Annot.span }
  type number = Terminals.Number.t
  type string = Terminals.String.t
  
  datatype value' = ValueString of string annot
    | ValueNumber of number annot
    | ValueObject of object
    | ValueArray of array
    | ValueTrue
    | ValueFalse
    | ValueNull
  and object' = ObjectObject of (member * member list) option
  and member' = MemberMember of string annot * value
  and array' = ArrayArray of (value * value list) option
  withtype value = value' annot
  and object = object' annot
  and member = member' annot
  and array = array' annot
  
  datatype terminal_token = TerminalNumber of Terminals.Number.t
  | TerminalString of Terminals.String.t
  
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
        , (fn (s , p) =>
            case Terminals.String.lex (s , p) of
              SOME (v , s' , p') => SOME (TerminalString v , s' , p')
            | NONE => NONE)
        ]
    end
    val keywords =
      [ ("," , 0)
      , (":" , 1)
      , ("[" , 2)
      , ("]" , 3)
      , ("false" , 4)
      , ("null" , 5)
      , ("true" , 6)
      , ("{" , 7)
      , ("}" , 8)
      ]
  )
  open Internal
  
  val parseTerminalNumber = parseTerminal (fn TerminalNumber v => SOME v | _ => NONE)
  val parseTerminalString = parseTerminal (fn TerminalString v => SOME v | _ => NONE)
    val parseValueDummy : value t_dummy = dummy ()
    val parseObjectDummy : object t_dummy = dummy ()
    val parseMemberDummy : member t_dummy = dummy ()
    val parseArrayDummy : array t_dummy = dummy ()
  
  val lex = lex
  
    (* Value *)
    val parseValue =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseNull =
            create (fn () => ValueNull) (
              bind (keyword 5) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseFalse =
            create (fn () => ValueFalse) (
              bind (keyword 4) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseTrue =
            create (fn () => ValueTrue) (
              bind (keyword 6) (fn v0 =>
              return_node () [ annot_add v0 ]))
  
          val parseArray =
            create ValueArray (
              bind (parseNonterminal (deref parseArrayDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseObject =
            create ValueObject (
              bind (parseNonterminal (deref parseObjectDummy)) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseNumber =
            create ValueNumber (
              bind (parseTerminalNumber) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
          val parseString =
            create ValueString (
              bind (parseTerminalString) (fn v0 =>
              return_node (#node v0) [ annot_add v0 ]))
  
        in either
        [ parseNull
        , parseFalse
        , parseTrue
        , parseArray
        , parseObject
        , parseNumber
        , parseString
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Object *)
    val parseObject =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseObject =
            create ObjectObject (
              bind (keyword 7) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (parseNonterminal (deref parseMemberDummy)) (fn v3 =>
                  bind (starLongest (
                    bind (
                      bind (keyword 0) (fn v6 =>
                      bind (parseNonterminal (deref parseMemberDummy)) (fn v7 =>
                      return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                    (fn v5 =>
                    return_node (#node v5) [ annot_add v5 ])))
                  (fn v4 =>
                  return_node ((#node v3) , (#node v4)) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 8) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseObject
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Member *)
    val parseMember =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseMember =
            create MemberMember (
              bind (parseTerminalString) (fn v0 =>
              bind (keyword 1) (fn v1 =>
              bind (parseNonterminal (deref parseValueDummy)) (fn v2 =>
              return_node ((#node v0) , (#node v2)) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseMember
        ]
        end)
  
      in
        forget parseAtom
      end
  
    (* Array *)
    val parseArray =
      let
        val parseAtom = fix (fn parseAtom =>
        let
          val parseArray =
            create ArrayArray (
              bind (keyword 2) (fn v0 =>
              bind (optionalLongest (
                bind (
                  bind (parseNonterminal (deref parseValueDummy)) (fn v3 =>
                  bind (starLongest (
                    bind (
                      bind (keyword 0) (fn v6 =>
                      bind (parseNonterminal (deref parseValueDummy)) (fn v7 =>
                      return_node (#node v7) [ annot_add v6 , annot_add v7 ])))
                    (fn v5 =>
                    return_node (#node v5) [ annot_add v5 ])))
                  (fn v4 =>
                  return_node ((#node v3) , (#node v4)) [ annot_add v3 , annot_add v4 ])))
                (fn v2 =>
                return_node (#node v2) [ annot_add v2 ])))
              (fn v1 =>
              bind (keyword 3) (fn v2 =>
              return_node (#node v1) [ annot_add v0 , annot_add v1 , annot_add v2 ]))))
  
        in either
        [ parseArray
        ]
        end)
  
      in
        forget parseAtom
      end
  
  
  val () = set parseValueDummy parseValue
  val () = set parseObjectDummy parseObject
  val () = set parseMemberDummy parseMember
  val () = set parseArrayDummy parseArray
  
  val parse = parser

end

functor JsonPrint (
  structure Ast : JSON_AST
  structure Terminals : sig
    structure Number : PRINT_TERMINAL where type t = Ast.number
    structure String : PRINT_TERMINAL where type t = Ast.string
  end
) :>
sig
  val printNumber : Ast.number Ast.annot -> string
  val printString : Ast.string Ast.annot -> string
  val printValue : Ast.value -> string
  val printObject : Ast.object -> string
  val printMember : Ast.member -> string
  val printArray : Ast.array -> string
  val prettyPrintNumber : Ast.number Ast.annot -> string
  val prettyPrintString : Ast.string Ast.annot -> string
  val prettyPrintValue : Ast.value -> string
  val prettyPrintObject : Ast.object -> string
  val prettyPrintMember : Ast.member -> string
  val prettyPrintArray : Ast.array -> string
  
end = struct

  open Ast
  
  val push = PrintBuffer.push
  
  fun printNumber buf
    ( { node , span = { start = { lineno , ... } , ... } } : number annot ) =
    push buf (Terminals.Number.show node) lineno
  
  fun printString buf
    ( { node , span = { start = { lineno , ... } , ... } } : string annot ) =
    push buf (Terminals.String.show node) lineno
  
  fun printValue buf
    ( { node , span = { start = { lineno , ... } , ... } } : value ) =
    case node of
        ValueString v0 =>
          ( push buf "ValueString" lineno
          ; push buf "(" lineno
          ; printString buf v0
          ; push buf ")" lineno
          )
      | ValueNumber v0 =>
          ( push buf "ValueNumber" lineno
          ; push buf "(" lineno
          ; printNumber buf v0
          ; push buf ")" lineno
          )
      | ValueObject v0 =>
          ( push buf "ValueObject" lineno
          ; push buf "(" lineno
          ; printObject buf v0
          ; push buf ")" lineno
          )
      | ValueArray v0 =>
          ( push buf "ValueArray" lineno
          ; push buf "(" lineno
          ; printArray buf v0
          ; push buf ")" lineno
          )
      | ValueTrue =>
          ( push buf "ValueTrue" lineno
          )
      | ValueFalse =>
          ( push buf "ValueFalse" lineno
          )
      | ValueNull =>
          ( push buf "ValueNull" lineno
          )
  
  and printObject buf
    ( { node , span = { start = { lineno , ... } , ... } } : object ) =
    case node of
        ObjectObject v0 =>
          ( push buf "ObjectObject" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => let val (v0v0 , v0v1) = v0v in push buf "(" lineno ; printMember buf v0v0 ; push buf " , " lineno ; ( push buf "[" lineno ; List.appi (fn ( i , v0v1e ) => ( if i > 0 then push buf " , " lineno else () ; printMember buf v0v1e )) v0v1 ; push buf "]" lineno ) ; push buf ")" lineno end)
          ; push buf ")" lineno
          )
  
  and printMember buf
    ( { node , span = { start = { lineno , ... } , ... } } : member ) =
    case node of
        MemberMember (v0 , v1) =>
          ( push buf "MemberMember" lineno
          ; push buf "(" lineno
          ; printString buf v0
          ; push buf " , " lineno
          ; printValue buf v1
          ; push buf ")" lineno
          )
  
  and printArray buf
    ( { node , span = { start = { lineno , ... } , ... } } : array ) =
    case node of
        ArrayArray v0 =>
          ( push buf "ArrayArray" lineno
          ; push buf "(" lineno
          ; (case v0 of NONE => push buf "_" lineno | SOME v0v => let val (v0v0 , v0v1) = v0v in push buf "(" lineno ; printValue buf v0v0 ; push buf " , " lineno ; ( push buf "[" lineno ; List.appi (fn ( i , v0v1e ) => ( if i > 0 then push buf " , " lineno else () ; printValue buf v0v1e )) v0v1 ; push buf "]" lineno ) ; push buf ")" lineno end)
          ; push buf ")" lineno
          )
  
  
  fun print f = fn v =>
  let val buf = PrintBuffer.empty ()
  in f buf v
  ; PrintBuffer.toString buf
  end
  val printNumber = print printNumber
  val printString = print printString
  val printValue = print printValue
  val printObject = print printObject
  val printMember = print printMember
  val printArray = print printArray
  
  
  fun prettyPrintNumber buf
    ( { node , span = { start = { lineno , ... } , ... } } : number annot ) =
    push buf (Terminals.Number.show node) lineno
  
  fun prettyPrintString buf
    ( { node , span = { start = { lineno , ... } , ... } } : string annot ) =
    push buf (Terminals.String.show node) lineno
  
  fun prettyPrintValue buf
    ( { node , span = { start = { lineno , ... } , ... } } : value ) =
    let val prettyPrintSelf = prettyPrintValue
    in
    case node of
        ValueNull =>
          ( push buf "null" lineno)
      | ValueFalse =>
          ( push buf "false" lineno)
      | ValueTrue =>
          ( push buf "true" lineno)
      | ValueArray v0 =>
          ( prettyPrintArray buf v0)
      | ValueObject v0 =>
          ( prettyPrintObject buf v0)
      | ValueNumber v0 =>
          ( prettyPrintNumber buf v0)
      | ValueString v0 =>
          ( prettyPrintString buf v0)
    end
  
  and prettyPrintObject buf
    ( { node , span = { start = { lineno , ... } , ... } } : object ) =
    let val prettyPrintSelf = prettyPrintObject
    in
    case node of
        ObjectObject v1 =>
          ( push buf "{" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val (v3 , v4) = v2
                  in prettyPrintMember buf v3
                  ; List.app (fn v5 =>
                      ( let val v7 = v5
                          in push buf "," lineno
                          ; prettyPrintMember buf v7
                          end)) v4
                  end))
          ; push buf "}" lineno)
    end
  
  and prettyPrintMember buf
    ( { node , span = { start = { lineno , ... } , ... } } : member ) =
    let val prettyPrintSelf = prettyPrintMember
    in
    case node of
        MemberMember (v0 , v2) =>
          ( prettyPrintString buf v0
          ; push buf ":" lineno
          ; prettyPrintValue buf v2)
    end
  
  and prettyPrintArray buf
    ( { node , span = { start = { lineno , ... } , ... } } : array ) =
    let val prettyPrintSelf = prettyPrintArray
    in
    case node of
        ArrayArray v1 =>
          ( push buf "[" lineno
          ; (case v1 of NONE => ()
              | SOME v2 =>
              ( let val (v3 , v4) = v2
                  in prettyPrintValue buf v3
                  ; List.app (fn v5 =>
                      ( let val v7 = v5
                          in push buf "," lineno
                          ; prettyPrintValue buf v7
                          end)) v4
                  end))
          ; push buf "]" lineno)
    end
  
  
  val prettyPrintNumber = print prettyPrintNumber
  val prettyPrintString = print prettyPrintString
  val prettyPrintValue = print prettyPrintValue
  val prettyPrintObject = print prettyPrintObject
  val prettyPrintMember = print prettyPrintMember
  val prettyPrintArray = print prettyPrintArray
  
end