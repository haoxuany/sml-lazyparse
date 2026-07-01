# sml-lazyparse - A lexer and parser generator for lazy people

There are excellent lexer and parser generator libraries out there today for 
Standard ML. Most of the time,

- If you need a good lexer and LR parser without shenanigans,
kcrary's [cmtool](https://github.com/kcrary/cmtool) is excellently written and very efficient.
- Otherwise, if you need an LL(k) parser, the [ml-lpt](https://www.smlnj.org/doc/ml-lpt/manual.pdf)
library from the New Jersey compiler is also very well written.

This library is for everyone else who **hates** writing lexing and parsing.

Unlike the approach in [the cmtool paper](https://www.cs.cmu.edu/~crary/papers/2018/cmtool.pdf) where 
the "disembodied code" problem is dealt with through generating intermediate types and functorizing them,
my approach to the "disembodied code" problem is to say screw all this and just generate all 
the parsing code away completely. Instead of writing parsing code, I will just give you a tree and 
you can process away whatever you don't need (which, let's be honest, most compilers need a second pass in 
parsing today due to stuff that isn't context free [ex. fixity] anyway).

As such, the explicit design goal of this project is to **minimize** the effort you need to get a working
lexer and parser and to be able to do this as quickly as possible. This README serves as a manual 
for usage and is kept intentionally short, so you can spend your life writing stuff that isn't 
a lexer or parser.

The tradeoff here is that some design decisions are made for you. In particular, it assumes that:
- You know your grammar ahead of time, and it is context-free, or at least a significant subset of it is (ex. Standard ML).
- You are okay with a little bit of a performance penalty (in realistic scenarios, not that much worse than a handwritten lexer/parser),
and quite a bit more of a memory penalty (due to dependence on Johnson 1995).
- The generated tree covers your use case (which it should, most of the time).
If you need something like a complex error recovery partial parser, you are better off writing one by hand.
- You are okay with handling ambiguity in grammar, to the extent that you care. This library gives you all possible valid parses in cases of ambiguity.

On the flip side, you gain:
- Very easy modification to the grammar, most of the time modifications are a mere few lines and are immediately intuitive.
- No longer necessary to deal with left factoring/shift-reduce conflicts a lot of the time.
- A default pretty printer and repl built for you for testing your grammar tree and lexer, so you can spot mistakes almost instantly.

All of this resulting in much faster development cycles.

## Building

I will assume that you are using the Standard ML of New Jersey compiler. Other compiler support (ex. mlton) will be coming in the future.

Cloning the repo is required, because it contains both a binary (for generator) and a library that the generated code needs to be linked against.
As such you will probably want to clone this in the project tree you are using it. This also contains submodules, hence:

```sh
git clone https://github.com/haoxuany/sml-lazyparse
cd sml-lazyparse/
git submodule update --init --recursive
make
```

This produces `sml-lazyparse/bin/lazyparse`. We will call the binary `lazyparse` for future reference.

## A quick tutorial

This will take roughly 10 mins to read and implement. I will assume you have some familiarity with basic terminology of parsing (if not, consult your undergrad freshman course on computability theory).

We will start with a simple case: writing a parser for [JSON](https://www.json.org/json-en.html). This is particularly simple because there is no ambiguity in the grammar.

There are exactly 2 things you need for writing a parser:
1. A grammar file (`.ungram`) that specifies parsing rules.
2. Code that specifies lexing for more complex terminals and whitespace (that are not keywords). This is *just* regular Standard ML code.

This example is under examples/json/ if you want to take a look. There are exactly 3 steps.

### Step 1: Write the Grammar File

Unlike most lexer/parser generator libraries, we always start with the grammar first. We create a `.ungram` file:

[json.ungram](examples/json/json.ungram)
```
// Tutorial Example

Value ::= 'string' [ name String ]
        | 'number' [ name Number ]
        | Object [ name Object ]
        | Array [ name Array ]
        | "true" [ name True ]
        | "false" [ name False ]
        | "null" [ name Null ]

Object ::= "{" (Member ("," Member)*)? "}" [ name Object ]

Member ::= 'string' ":" Value [ name Member ]

Array ::= "[" (Value ("," Value)*)? "]" [ name Array ]
```

Note that the grammar diverges significantly from ungrammar itself, so knowing ungrammar syntax is not useful/necessary. To give a quick explanation:

- Line comments start with `//` and are ignored. `/* ... */` are multiline comments and can be nested.
- Nonterminals always start with an Uppercase letter and can contain any letter, digit, or underscore. (ex. `Value`).
- There are two kinds of terminals:
  + Keyword terminals are double quoted (we will call them *keywords* for short). This tells the lexer to turn it into a keyword lexed exactly as written and to be parsed as a keyword in parsing. (ex. `"true"`)
  + Other terminals are single quoted (we will call them *terminals* for short). You will fill in how lexing is supposed to happen in Step 3. (ex. `'number'`)
- A nonterminal is parsed through a number of parsing rules. The general syntax for defining parsing a nonterminal is:
```
Nonterminal ::= rule1 [ name Name1 , other properties ]
             | rule2 [ name Name2 , other properties ]
             ...
```
- `[ ]` is a property list. For all the rules, `name` is a required property. There are other properties that can be added and will be explained later.
- Putting two tokens one after the other sequences them (ex. `'string' ":" Value` means to parse the terminal 'string', then the keyword ":", then some nonterminal Value).
- You can group tokens with parentheses (ex. `("," Member)`). `?` means to optionally parse one of the previous tokens (ex. `("," Member)?`). `*` means to parse any number of them (including 0) (ex. `("," Member)*`). `+` means to parse one or more of them (ex. `("," Member)+`).
- Unlike ungrammar, you are *not* allowed to have `|` within a rule (you need to drag it out to a separate nonterminal), nor is the `label:A` syntax supported.
The reason will be obvious when looking at the generated code.
- The name of nonterminals and other terminals *should not clash with any keywords* of Standard ML or any commonly used types/functions in the generated code, for both uppercase and lowercase.
You have been warned, since otherwise the generated code will not compile.

### Step 2: Run Codegen

Then, run `lazyparse` that was built in the steps previously:
```
lazyparse examples/json/json.ungram
```

This will by default generate a [examples/json/json.sml](examples/json/json.sml).

The generated code gives you 4 things by default:
- The type of the parse tree, that also contains annotations on positions in the tree (span of expressions).
```
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
  and repl' = ReplValue of value
  withtype value = value' annot
  and object = object' annot
  and member = member' annot
  and array = array' annot
  and repl = repl' annot
  
end
```
- A functor that constructs a lexer and all parsers, given the relevant lexing code of terminals and whitespaces. 
```
functor JsonParser (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Number : TERMINAL
    structure String : TERMINAL
  end
) :>
sig
  include JSON_AST
  where type number = Terminals.Number.t
  where type string = Terminals.String.t
  
  type 'a parser
  
  datatype terminal_token = TerminalNumber of number
  | TerminalString of string
  
  structure TokenStream : sig
    type t
    datatype token =
      Keyword of String.string * Annot.span
    | Terminal of terminal_token * Annot.span
    
    datatype front = Nil | Cons of token * t
    val front : t -> front
    
    val pos : t -> Annot.pos
  end
  
  exception LexError of Char.char * Annot.pos
  val lex : Char.char Stream.stream -> Annot.pos -> TokenStream.t
  
  val parseValue : value parser
  val parseObject : object parser
  val parseMember : member parser
  val parseArray : array parser
  val parseRepl : repl parser
  
  datatype 'a result =
    Success of ('a * TokenStream.t) list
  | Fail of ParseError.t
  val parse : 'a parser -> TokenStream.t -> 'a result
  
end
```
- A pretty printer for the tree, given printing for terminals:
```
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
  val printRepl : Ast.repl -> string
  
  val prettyPrintNumber : Ast.number Ast.annot -> string
  val prettyPrintString : Ast.string Ast.annot -> string
  val prettyPrintValue : Ast.value -> string
  val prettyPrintObject : Ast.object -> string
  val prettyPrintMember : Ast.member -> string
  val prettyPrintArray : Ast.array -> string
  val prettyPrintRepl : Ast.repl -> string
  
end
```
- And a repl for testing:
```
functor JsonRepl (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Number : REPL_TERMINAL
    structure String : REPL_TERMINAL
  end
) :> sig
  val run : unit -> unit
end
```

For this tutorial, we will focus on using the Repl for quickly testing that 
your lexing and parsing works. This is what I recommend doing first 
since it allows you to see mistakes early on.


### Step 3: Implement Lexing for (Other) Terminals

Functorizing the generated code involves implementing one of the terminal
signatures. There are 3 terminal signatures commonly used:
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

Basically you have some abstract type `t` (that gets injected into the tree
where appropriate), and all you need to do is to implement how these terminals
are supposed to be lexed (and a `show` function for printing the terminal, used
in the print functor and repl for feedback).

The `lex` function makes use of a `LexStream`, with this signature:
```
structure LexStream : sig
  type stream

  datatype front = Nil | Cons of char * stream

  val front : stream -> front
  val pos : stream -> Annot.pos

end
```
This is just a monomorphic stream, that also keeps track of positions internally.

Going back to the REPL functor, for each terminal there is a terminal structure 
generated with its own lexing rules.

There is also a `Trivial` structure. This is 
basically a "whitespace" terminal. We allow these trivial tokens to occur basically
anywhere in the program, and as of writing, these trivial tokens get ignored during
parsing. Nevertheless, we still need to write lexing rules for it to explain what
constitutes whitespace.
```
functor JsonRepl (
  structure Trivial : TERMINAL
  structure Terminals : sig
    structure Number : REPL_TERMINAL
    structure String : REPL_TERMINAL
  end
) :> sig
  val run : unit -> unit
end
```

Let's start by implementing the lexer for trivial/whitespace tokens. 
We can do this manually:

```
structure Run = struct
  structure LS = LexStream

  structure Trivial = struct
    type t = unit

    fun lex s =
      case LS.front s of
        LS.Nil => NONE
      | LS.Cons ( c , s ) =>
        if Char.isSpace c then
          let
            fun eatSpace s =
              case LS.front s of
                LS.Nil => s
              | LS.Cons ( c , tail ) =>
                  if Char.isSpace c then
                    eatSpace tail
                  else s
            val s = eatSpace s
          in
            SOME ( () , s )
          end
        else NONE
  end
end
```

This is fine, but we can do this in an easier way. This project comes with a 
`Regex` library for dealing with streams and works with UTF-8.
Since the whitespaces are actually regular, we can just use a regex:
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

Hence:
```
structure Trivial = struct
  type t = unit
  
  structure R = Regex

  val regex = R.plus ( R.set [ #" ", #"\n" , #"\t" , #"\r" ] )
  fun lex s =
    case R.regex regex s of
      NONE => NONE
    | SOME ( _ , s ) => SOME ( () , s )
end
```

Which is easier. 

We can do the same for the terminals Number and String. There is a small 
collection of commonly used lexing functions already available in 
[src/link/lex_common.sml](src/link/lex_common.sml). Hence the final product is 
(assuming that we are representing numbers with just strings for now):

[examples/json/json_parser.sml](examples/json/json_parser.sml)
```
structure Run = struct

  local
    structure R = Regex

    val jsonNumber =
      let
        val digits = R.plus R.digit
        val sign = R.opt (R.exact #"-")
        val frac = R.seq [R.exact #"." , digits]
        val expo = R.seq [R.set [#"e" , #"E"] , R.opt (R.set [#"+" , #"-"]) , digits]
      in
        R.seq [sign , digits , R.opt frac , R.opt expo]
      end
  in
    structure JsonRepl = JsonRepl (
      structure Trivial = LexCommon.WhitespaceTrivial (
        val whitespace = [" " , "\t" , "\n" , "\r"]
      )
      structure Terminals = struct
        structure Number = LexCommon.RegexReplTerminal (
          type t = string
          val regex = jsonNumber
          fun map s = s
          fun show s = s
        )
        structure String = LexCommon.StringTerminal
      end
    )
  end

  val run = JsonRepl.run
end
```

Finally, we need to link the generated parser, the terminal lexers together.
The rule of thumb is that all these dependencies are required, and the link order 
is:
1. SML Basis Library.
2. link.cm within this root directory.
3. The generated code.
4. Your lexer code.

Hence,

[examples/json/json.cm](examples/json/json.cm)
```
Group is

  $/basis.cm
  ../../link.cm

  json.sml
  json_parser.sml
```

With this, we are done, with around just 50 lines of code we have a json parser.

### Running the repl

The provided repl gives you the parsed structure for testing purposes. This 
will be very helpful when dealing with ambiguous parses, which will be explained
in much more detail later.

The most important thing to remember is that the Repl by default only runs
parsing for the **last** nonterminal in the grammar file. As such, the example
json repl here will parse `Array` only. This is probably not what you want.
One thing you can do is to move `Value` down to the end of the file, but I would
instead recommend adding a dummy `Repl` token at the end so that you have finer
control of exactly which syntax are acceptable for the Repl. For example:

[json.ungram](examples/json/json.ungram)
```
// Tutorial Example

Value ::= 'string' [ name String ]
        | 'number' [ name Number ]
        | Object [ name Object ]
        | Array [ name Array ]
        | "true" [ name True ]
        | "false" [ name False ]
        | "null" [ name Null ]

Object ::= "{" (Member ("," Member)*)? "}" [ name Object ]

Member ::= 'string' ":" Value [ name Member ]

Array ::= "[" (Value ("," Value)*)? "]" [ name Array ]

Repl ::= Value [ name Value ]
```

Regenerate the code, and the Repl will start parsing the `ReplValue` rule.

An example of what happens when you run the Repl can look like:
```
> 10
parses (1):
0.  ReplValue ( ValueNumber ( 10 ) )
> { "10" : [ 20 , 30 ] }
parses (1):
0.  ReplValue ( ValueObject ( ObjectObject ( ( MemberMember ( "10"  ,  ValueArray ( ArrayArray ( ( ValueNumber ( 20 )  ,  [ ValueNumber ( 30 ) ] ) ) ) )  ,  [ ] ) ) ) )
```

Hence, the parsing works as expected for these cases.

For actual usage, you probably don't want the test Repl, in which case you should 
just use the generated JsonParser functor instead.

## Important References

While you are here, I would highly recommend reading [this](doc/details.md) for an example that goes into a much more in depth explanation 
of internal technical details, dealing with ambiguity, etc. The way ambiguous grammars are handled is markedly different from traditional
LL/LR parsers.

See [this](doc/reference.md) for a reference for important signatures that are exposed to the user.

See [this](doc/design.md) if you are wondering why certain decisions are made in certain ways in the library.


## License

This project is under MIT License.

This project depends on sml-parcom, which is also under MIT License.

This project depends heavily on cmlib, especially on the implementation of
Stream and SplayTree. cmlib is under MIT License.
