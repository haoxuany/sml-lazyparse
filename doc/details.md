# A complex example: handling arithmetic

This takes around 20 minutes to read, and covers some technical details in how certain rules are structured
and how to deal with ambiguous parses. It is highly likely you need to work with ambiguous grammars, and
it is important to know that this library deals with them in a very different way than most other parser generators.

Suppose that we are trying to write a parser for a simple arithmetic expression, with this grammar:

[examples/arith/arith.ungram](examples/arith/arith.ungram)
```
Exp ::= 
  Number [ name Number ]
| Exp "+" Exp [ name Plus ]
| Exp "*" Exp [ name Times ]
| "(" Exp ")" [ name Group ]

Number ::= 'nat' [ name Nat ]

Repl ::= Exp [ name Exp ]
```

and with this lexing implementation:
[examples/arith/arith_parser.sml](examples/arith/arith_parser.sml)
```

structure Run = struct

  local
    structure R = Regex
  in
    structure ArithRepl = ArithRepl (
      structure Trivial = LexCommon.WhitespaceTrivial (
        val whitespace = [" " , "\t" , "\n" , "\r"]
      )
      structure Terminals = struct
        structure Nat = LexCommon.RegexReplTerminal (
          type t = int
          val regex = R.plus R.digit
          fun map s =
            case Int.fromString s of
              NONE => raise Fail "Impossible"
            | SOME v => v
          val show = Int.toString
        )
      end
    )
  end

  val run = ArithRepl.run
end
```
and linking file:
[examples/arith/arith.cm](examples/arith/arith.cm)
```
Group is

  $/basis.cm
  ../../link.cm

  arith.sml
  arith_parser.sml
```

So far everything up here is self-explanatory. 

## Ambiguity

For this grammar, there is a particular problem that it is highly ambiguous. 
For a most simple example of rewriting, we can have:
```
Exp
=> Exp "+" Exp ( applying rule Plus )
=> ( Exp "+" Exp ) "+" Exp ( applying rule Plus on the first Exp )
=> ( 10 "+" 20 ) "+" 30 ( applying rule Number on all Exp )
```
Similarly,
```
Exp
=> Exp "+" Exp ( applying rule Plus )
=> Exp "+" ( Exp "+" Exp ) ( applying rule Plus on the second Exp )
=> 10 "+" ( 20 "+" 30 ) ( applying rule Number on all Exp )
```

Hence, for the expression `10 "+" 20 "+" 30`, we have two different parses. A 
similar situation occurs for `10 "+" 20 "*" 30`.

*Usually*, this library will give you all possible parses. However, there are 
**exceptions** for infix, and cases of `*`, `?`, and `+`.
The reason for these exceptions is that in practice these ambiguous parses are almost never desirable.
As such, there are a list of rules that this library follows to generate code of certain behavior.


### Infix

If a rule for parsing a nonterminal `E` is of the form `E .... E` (starts and ends with the same `E`),
and it is *not* of the form `E` itself (writing `E ::= E` is always a bug), then call this rule an
*infix* rule. For example, the `Plus` rule above is an infix rule (`Exp "+" Exp`).

For infix rules, there is a notion of **associativity** and **precedence**.

**Precedence** states the relationship between rules. A rule with a higher precedence will have higher binding priority (following Agda/Haskell conventions). For example, I want `10 + 20 * 30` to parse as `10 + ( 20 * 30 )`.
The `Times` rule need to bind tighter than the `Plus` rule and have a higher priority. As such, we can modify
the grammar explicitly by assigning a precedence:
```
Exp ::= 
  Number [ name Number ]
| Exp "+" Exp [ name Plus , prec 30 ]
| Exp "*" Exp [ name Times , prec 40 ]
| "(" Exp ")" [ name Group ]

Number ::= 'nat' [ name Nat ]

Repl ::= Exp [ name Exp ]
```

This gives us a correct parse for "10 + 20 * 30":
```
Use \ to continue string in new line at EOL and \\ for the literal backslash at EOL
> 10 + 20 * 30
parses (1):
0.  ReplExp ( ExpPlus ( ExpNumber ( NumberNat ( 10 ) )  ,  ExpTimes ( ExpNumber ( NumberNat ( 20 ) )  ,  ExpNumber ( NumberNat ( 30 ) ) ) ) )
```

**Associativity** states how parsing is grouped between two infix rules with the same precedence. The most common case of this is using the
same infix rule twice, for example: `10 + 20 + 30`, which is two applications of the same rule (and hence the same precedence).

We want this to be grouped to the left, and hence this should be left associative (`(10 + 20) + 30`).
We can add to the grammar an explicit property (`left`, `right`, `none`):
```
Exp ::= 
  Number [ name Number ]
| Exp "+" Exp [ name Plus , prec 30 , assoc left ]
| Exp "*" Exp [ name Times , prec 40 , assoc left ]
| "(" Exp ")" [ name Group ]

Number ::= 'nat' [ name Nat ]

Repl ::= Exp [ name Exp ]
```

Note that for infix rules, by default it is assigned **left associative with precedence of 50**. This default is chosen because it removes ambiguity for
infix/infix interactions. You can override the associativity and precedence by adding a property.

**One important caveat**: if the first/last token is not *exactly* the same as the nonterminal, then that rule will not be treated as infix!
For example:
```
E ::= E "+" E? [ name PlusMaybe ]
```
This rule is not treated as infix (it is in fact, postfix), because the last token is a `E?` instead of `E`. If you want to still have infix 
behavior, it is important to break down the rule into two separate ones, and assign the actual infix one precedence/associativity:
```
E ::= E "+" [ name PlusMaybe ]
    | E "+" E [ name Plus , assoc left , prec 30 ]
```

The default behavior of the parser is to always pick the **longest parses** when possible, hence `10 + 20` will always be parsed with `Plus`.

### Prefix/Postfix

If a rule for parsing a nonterminal `E` is of the form `N ... E` (ends with the same `E`, but starts with something that is not `E`), then
we call the rule prefix. Similarly, a rule of the form `E ... N` is called postfix.

Postfix and prefix rules have no associativity (because it cannot interact with itself to create ambiguous parses). They do have a notion
of precedence. For example, if my grammar is:
```
Exp ::= 
  Number [ name Number ]
| Exp "+" Exp [ name Plus , prec 30 , assoc left ]
| "succ" Exp [ name Succ ]
```

Then `Succ` and `Plus` can interact in ambiguous ways, for example, `succ 1 + 2` can be parsed as either `(succ 1) + 2` or `succ (1 + 2)`.

By default, all prefix/postfix are assign **a precedence of 50**. Any attempt at giving a prefix/postfix rule associativity will throw an error.


### Nonfix

If a rule for parsing a nonterminal `E` is of the form `N ... N` (neither start nor end with `E`, can be a single token). Then it is nonfix.
Nonfix does *not* have a notion of precedence nor associativity. Because it cannot interact with other rules in ambiguous ways at all.

Any attempt at giving a nonfix rule precedence or associativity will throw an error.

### Meaning of ?, +, *, and selection of rules

Note that using `?`, `+` or `*` will always result in the *longest* match in all cases. They do not produce ambiguity.

In the case that more than a single rule can be applied at a time, the one with the *longest parse* (aka, eats the most amount of tokens) will be kept.
If there are multiple parses with the same amount of tokens taken, all of them will be kept. This creates ambiguous parses. For example:
```
Exp ::= 
  Number [ name Number ]
| Exp "+" Exp [ name Plus , prec 30 ]
| Exp "+" Exp [ name Plus2 , prec 30 ]
| Exp "*" Exp [ name Times , prec 40 ]
| "(" Exp ")" [ name Group ]

Number ::= 'nat' [ name Nat ]

Repl ::= Exp [ name Exp ]
```

There are two rules `Plus` and `Plus2` with the same parse, this gives us ambiguity on which rule to apply, as they eat up the entire expression in both cases:
```
> 10 + 10
parses (2):
0.  ReplExp ( ExpPlus ( ExpNumber ( NumberNat ( 10 ) )  ,  ExpNumber ( NumberNat ( 10 ) ) ) )
1.  ReplExp ( ExpPlus2 ( ExpNumber ( NumberNat ( 10 ) )  ,  ExpNumber ( NumberNat ( 10 ) ) ) )
```


## Errors

Right now the implementation has a very naive way of error handling. For lex errors, it can happen when the lex token is neither a keyword nor trivial nor one of the terminals. For example,
```
> 1 + "test"
lex error at 1:5: '"test"'
```

For parse errors, there are 3 kinds, all defined in the `ParseError` structure:
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

For example,
```
> + 1
parse error:
  Number: Nat: expected nat but got '+' at 1:1
```

Note that this can often be potentially confusing. Error reporting is at a somewhat experimental stage at this point.

The most confusing behavior today has to do with partial parses, for example:
```
> 1 + +
parses (1):
0.  ReplExp ( ExpNumber ( NumberNat ( 1 ) ) ) (2 trailing)
```

You might expect this to syntax error, but it does not. In fact, it does a best effort longest parse,
which matches the prefix `1` using the `Exp -> Number -> Nat` rule. It leaves 2 trailing tokens, but it does not error.

In other parse generators they would often have a `follow` specification to explain which tokens are allowed to follow
the parse (which would have ruled out `+` as a follow token). There are some reasons to not do so here, part of it is that 
unlike most parse generators, I am actually exposing all parse functions for all nonterminals and terminals, so you would require
follow tokens everywhere. This would also require me to invent some syntax to make this work, so the situation is bad either way.

Note that in the generated signature, the token stream is in fact exposed to you to do this sort of check:
```
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

  datatype 'a result =
    Success of ('a * TokenStream.t) list
  | Fail of ParseError.t
  val parse : 'a parser -> TokenStream.t -> 'a result
```

As such, in the worst case, you can always check the remaining token stream to figure out what is left.

## Using the generated parser

The generated parser has this signature:
```
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
```

Once you figured out you are free of grammar bugs, you can just use the parser
directly without the Repl stuff compiled in. The types should be self explanatory,
but `Annot` requires a bit of a clarification on what it does.

The signature of `Annot` is:
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

The point of `Annot` is to keep track of (file) positions in the stream. `Annot.pos`
is a single byte location of the file + the corresponding line and column numbers 1-indexed, 
and `span` is a range.

In the generated signature, all rules are annotated with a span. The span of the rule is the span from 
the first nonempty token to the last. This includes keywords that are *not* reflected in the generated tree.

If for some reason you need the span of a certain keyword, one trick is to factor out the keyword to a nonterminal so that annotation gets added. For example:
```
Plus ::= "+" [ name Symbol ]

Exp ::= 
  Number [ name Number ]
| Exp Plus Exp [ name Plus , prec 30 ]
```

## Lexer Rules

One more detail about lexing: in short, lexing priority works basically the same way as every other lexer generator.

There are 3 lexing classes: keywords (the keyword literals from the grammar file), trivial (`structure Trivial`), and other terminals (`structure Number`, etc.).

In theory, it is assumed that the lexing functions and lexing classes for each are distinct from each other (or at least as much as possible). In practice, the specific lexing class is selected through:

- In the case of multiple results from the same stream, we pick the *longest one*.
- In the case where these are of the same length, we prioritize lexing it as a *keyword or trivial*, over other terminals (aka keyword literals in text are always keywords).
- In the case where multiple other terminal lexing classes have the same length, the selection of exactly which terminal it gets lexed to should be treated as **random** (this is not technically true, but should be treated as such). Hence your lexing classes must be distinct, or lexing becomes unpredictable.

For example, if I have a keyword `"if"` in my grammar file, and a regex lexer for ID terminals for alphabetical words, then seeing an "if" in the file classifies it as a keyword. Seeing a "iffy" in the file classifies it as a ID terminal. If you have a LONGID terminal that also happens to parse alphabetical words, then either ID or LONGID can be selected (and should be treated as random). As such, you will need to either remove the ID terminal to just use LONGID, or have them lex mutually exclusive things. Most people generally pick the former.

Moreover, the classic parser trick for allowing certain keywords as a value is to basically add them to a nonterminal class, for example. If I want to use "if" as an ID in an expression, I can do something like:
```
ExpId ::= 'id' [ name Id ]
        | "if" [ name If ]

Exp ::= "if" E "then" E "else" E [ name IfThenElse ]
```

And then process these cases as a second pass in the tree.
