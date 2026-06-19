signature TERMINAL =
sig
  type t
  type 'a stream

  val lex : (char stream * Annot.pos) -> (t * char stream * Annot.pos) option
end

signature TERMINAL_PRINTABLE =
sig
  include TERMINAL
  
  val show : t -> string
end
