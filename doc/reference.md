# Reference

These are common structures and signatures exposed in `link.cm` that you will find useful.
These signatures are almost all self explanatory.

## Stream

From cmlib. `structure Stream : STREAM`.

```
signature STREAM =
sig

  type 'a stream
  datatype 'a front = Nil | Cons of 'a * 'a stream

  val front : 'a stream -> 'a front
  val eager : 'a front -> 'a stream
  val lazy : (unit -> 'a front) -> 'a stream

  val fromProcess : (unit -> 'a option) -> 'a stream
  val fromList : 'a list -> 'a stream
  val fromLoop : ('a -> ('a * 'b) option) -> 'a -> 'b stream
  val fromTable : ('a * int -> 'b) -> 'a -> int -> 'b stream

  val fromString : string -> char stream
  val fromBytestring : Bytestring.string -> Word8.word stream
  val fromTextInstream : TextIO.instream -> char stream
  val fromBinInstream : BinIO.instream -> Word8.word stream

  val fix : ('a stream -> 'a stream) -> 'a stream

  exception Empty
  val hd : 'a stream -> 'a
  val tl : 'a stream -> 'a stream
  val @ : 'a stream * 'a stream -> 'a stream
  val take : 'a stream * int -> 'a list
  val drop : 'a stream * int -> 'a stream
  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val app : ('a -> unit) -> 'a stream -> unit
  val fold : ('a * 'b Susp.susp -> 'b) -> 'b -> 'a stream -> 'b
  val toList : 'a stream -> 'a list

end
```

## Terminal Signatures

```
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
```

## Annot

```
structure Annot : sig 

  type pos =
    { pos : int (* byte position *)
    , lineno : int (* 1-indexed *)
    , colno : int (* 1-indexed *)
    }

  type span =
    { start : pos
    , finish : pos
    }

  val empty : pos
  val newline : int (* bytes *) -> pos -> pos
  val sameline : int (* bytes *) -> pos -> pos
  val span : pos -> pos -> span
  val join : span -> span -> span
  val length : span -> int
  val compare : pos * pos -> order

end
```

## LexStream

```
structure LexStream : sig
  type stream

  datatype front = Nil | Cons of char * stream

  val front : stream -> front
  val pos : stream -> Annot.pos

  val fromStream : char Stream.stream -> Annot.pos -> stream

end
```

This is a stream that tracks position.
Hence, even an empty stream has a position (whatever the position EOF is).

## Regex

```
structure Regex 
:> sig
  type t

  val epsilon : t

  val exact : char -> t
  val exactOrd : int -> t
  val exactUtf8 : string -> t
  (* int represents utf8 codepoints *)
  val matching : (int -> bool) -> t
  val any : t
  val except : (int -> bool) -> t

  val charRange : char * char -> t
  val ordRange : int * int -> t
  val utf8Range : string * string -> t

  val alt : t list -> t
  val seq : t list -> t
  val star : t -> t
  val plus : t -> t
  val opt : t -> t

  val string : string -> t
  val set : char list -> t

  val digit : t
  val lower : t
  val upper : t
  val alpha : t
  val alphaNum : t
  val whitespace : t

  val utf8 : string -> int

  val regex : t -> LexStream.stream -> (string * LexStream.stream) option

end
```

## LexCommon

```
structure LexCommon : sig

  functor RegexTerminal (
    type t
    val regex : Regex.t
    val map : string -> t
  ) : TERMINAL

  functor RegexReplTerminal (
    type t
    val regex : Regex.t
    val map : string -> t
    val show : t -> string
  ) : REPL_TERMINAL

  functor WhitespaceTrivial (
    val whitespace : string list
  ) : TERMINAL

  structure StringTerminal : REPL_TERMINAL

end
```

Just commonly used lexing functions. You can choose whether any of these are useful.

## ParseError

```
structure ParseError : sig 
  datatype t =
    UnexpectedEOF of 
      { sort : string 
      , rulename : string 
      , pos : Annot.pos 
      }
  | ExpectedKeyword of
      { sort : string 
      , rulename : string 
      , keyword : string
      , actual : string 
      , pos : Annot.pos 
      }
  | ExpectedTerminal of
      { sort : string 
      , rulename : string 
      , terminal : string
      , actual : string
      , pos : Annot.pos 
      }

  val show : t -> string
end
```
