signature TERMINAL =
sig
  type t
  type 'a stream

  val lex : (char stream * Annot.pos) -> (t * char stream * Annot.pos) option
end

signature PRINT_TERMINAL =
sig
  type t

  val show : t -> string
end
