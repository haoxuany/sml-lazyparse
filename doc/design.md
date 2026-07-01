# Design Rationale

In the process of writing this generator and library, I have adopted certain decisions in the design. Most of them I don't regret, but some of them I hesitated a bit on the decision and wondered if something could be done differently (still unclear to me).

In any case, it is somewhat important to explain why I started this library in the first place. The short version is: I hate writing lexing/parsing code, and it is slowing down my own project quite a bit. I have been working on a compiler in private that is meant to compile "a variation of" Standard ML. The goal here is to make drastic changes to the frontend and elaboration to better reflect the internal theory of Standard ML. 

Like most people, I couldn't care less about the syntax, but with elaboration the syntax matters, and the specifics can be decided much later. However, trying to make changes in a traditional lexer/parser setup is a huge pain: I need to remind myself all the things that are done to deal with ambiguity (left factoring/precedence/LR shift reduce conflict), and I need to do a bunch of transforms to make this work. It is mindless, tedious work, and the worst part is that you have to revert everything when your ideas don't work out. One week later I have to reread everything to remind myself exactly how certain parsing rules were written and why. The concrete syntax of SML has a bunch of these highly painful ambiguities. This distracts me from actually working on elaboration + backend, which is the actually important part of the compiler. The frontend can just be swapped if you don't like it.

In a stroke of luck at the time I was also looking into how Agda handles mixfix, which pointed me to (Johnson 1995). Hence the sml-parcom library and this are just byproducts of the algorithm, which is much more flexible than it looks.

## Lexing 

At first I thought I would simply write a stream that just takes in characters and produce the parse tree directly, bypassing a specific step for lexing (aka Coq-like). This is a very bad idea that I noticed immediately wouldn't work: because (Johnson 1995) is still at its core a backtracking parser. As such, you really don't want time complexity to blow up because you are pulling character by character (vs token by token).

Hence I settled on a separate lexing step, which led to the distinction between keywords (shows up in almost every grammar) and other terminals. The nice thing about a known list of keywords is that 1: you don't have to specify them again in the lexer/parser, and 2: you can look up the longest keyword through a simple trie implementation, and hence your time complexity lexing keywords is not dependent on the number of keywords anymore.

The unfortunate part is that I still have to make a decision on the other terminals. I've chosen to functorize to a lex function:
```
signature TERMINAL =
sig
  type t

  val lex : LexStream.stream -> (t * LexStream.stream) option
end
```

Just looking at the type will tell you that this is suboptimal, since failing to match the lexer typeclass means that we will have to backtrack to try the other lexer classes. So lexing a single token can be O(1) (for keywords) + O(k) (whitespace) + O(kp) (terminal length * number of terminals) = O(kp), where k is the length of what is lexed, so lexing the entire file can be O(np) at worst where n is the file length.

This is not great: this is exactly why most lexer generators use a DFA construction. However, if I were to do this construction, the lexer can no longer be a negative type like a function. This requires me to expose the specific lexing algorithm positively. I can't restrict this to just regular expressions either, since there are legit use cases for more complex lexing (ex. nested comments). So I will basically need to:
- build syntax for state machines
- collate these and then do a DFA construction

Aka implement it like most lexers, which requires basically a separate manual for writing state machines. This is something I'm trying to avoid since ideally I just write ML code, without writing a whole user manual for regex syntax, states, etc.

Hence, a sacrifice is made here for ease of use: this implementation just sticks to a backtracking lexer.

How bad is the time complexity anyway? O(np) is definitely way too conservative unless all your keywords, tokens and whitespace are single character lexes. Realistically, p isn't that high either. SML has about 7 syntactic classes, so I barely notice a performance bottleneck here at least.

## Parsing

The parsing algorithm is almost exactly the same as the one for mixfix parsing in sml-parcom. The transformation to work with associativity and precedence, while not difficult, is certainly very annoying to do. Dealing with associativity and precedence is in fact the bulk of the library.

I've added annotations because quite frankly I can't think of why anyone wouldn't want to have everything annotated by default. I see this in practically every compiler, just done more manually and painfully. I suppose if you don't care about error reporting or are writing network applications then this can be a waste of memory, and quite frankly in that case you wouldn't have used this library either way.

Similarly, the generated print functor and repl functors are there because the chances of not screwing up your grammar first try is quite low, so one way or the other you'll need something like this anyway. Since we already know the syntax, we can already generate sufficiently good defaults.
