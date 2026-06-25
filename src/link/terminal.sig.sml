
signature TERMINAL =
sig
  type t

  val lex : LexStream.stream -> (t * LexStream.stream) option
end

signature PRINT_TERMINAL =
sig
  type t

  val show : t -> string
end

signature REPL_TERMINAL =
sig
  type t

  val lex : LexStream.stream -> (t * LexStream.stream) option
  val show : t -> string
end
