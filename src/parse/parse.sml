
structure Parse : sig
  exception LexError of string

  type lex_stream

  val streamify : (unit -> string) -> lex_stream
  val streamifyReader : ('a -> (char * 'a) option) -> 'a -> lex_stream
  val streamifyInstream : TextIO.instream -> lex_stream
  val getPos : lex_stream -> int

  type sourcemap
  val mkSourcemap : unit -> sourcemap
  val mkSourcemap' : string -> sourcemap

  val parse : sourcemap -> lex_stream -> Ast.grammar

end = struct

  structure Parser = UngramParseFn(UngramLex)

  exception LexError of string

  open UngramLex

  type lex_stream = prestrm * yystart_state

  open AntlrStreamPos

  fun parse (sm : sourcemap) (strm : UngramLex.strm)
    : Ast.grammar =
    let
      val (result , _ , repairs) = Parser.parse (UngramLex.lex sm) strm
      val () =
        case repairs of
          nil => ()
        | _ => raise LexError
            (String.concat
              (List.map
                (fn repair =>
                  AntlrRepair.repairToString UngramTokens.toString sm repair ^ "\n")
                repairs))
    in
      case result of
        SOME grammar => grammar
      | NONE => raise LexError "unknown lexer error\n"
    end

end
