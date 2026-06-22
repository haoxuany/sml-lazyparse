%defs (

structure Tok = UngramTokens

val commentLevel : int ref = ref 0

type lex_result = Tok.token

fun eof () = 
( if !commentLevel > 0
  then raise Fail "unterminated comment on EOF"
  else ();

  Tok.EOF
)

local
  val text : string list ref = ref []
in

fun pushText s = (text := s :: (!text))
val text = fn () =>
  let
    val result = String.concat (List.rev (!text))
  in
    text := [] ;
    result
  end

end
);

%let eol = ("\n" | "\r\n" | "\r");
%let ws = ("\009" | "\011" | "\012" | " " | {eol});
%let alpha = [a-zA-Z];
%let digit = [0-9];
%let idchars = ({alpha} | {digit} | "_");
%let ident = {alpha}{idchars}*;

%states DOUBLEQUOTE SINGLEQUOTE COM;

%name UngramLex;

<INITIAL>{ws}+
        => (skip());

<INITIAL>"//"[^\n\r]*
        => (skip());

<INITIAL>"/*"
        => (commentLevel := 1; YYBEGIN COM; continue ());

<COM>"/*"
        => (commentLevel := !commentLevel + 1; continue ());
<COM>"*/"
        => (commentLevel := !commentLevel - 1;
            if !commentLevel = 0
              then (YYBEGIN INITIAL; continue ())
              else continue ());
<COM>. | {eol}
        => (continue ());

<INITIAL>"::="
        => (Tok.ColonColonEq);
<INITIAL>"|"    => (Tok.Bar);
<INITIAL>"*"    => (Tok.Star);
<INITIAL>"+"    => (Tok.Plus);
<INITIAL>"?"    => (Tok.Query);
<INITIAL>"("    => (Tok.LParen);
<INITIAL>")"    => (Tok.RParen);
<INITIAL>"["    => (Tok.LBracket);
<INITIAL>"]"    => (Tok.RBracket);
<INITIAL>","    => (Tok.Comma);

<INITIAL>"\""
        => (YYBEGIN DOUBLEQUOTE; continue());
<DOUBLEQUOTE>"\""
        => (YYBEGIN INITIAL; Tok.StringLiteral (text()));
<DOUBLEQUOTE>"\\\""
        => (pushText "\""; continue());
<DOUBLEQUOTE>"\\\\"
        => (pushText "\\"; continue());
<DOUBLEQUOTE>"\\n"
        => (pushText "\n"; continue());
<DOUBLEQUOTE>"\\t"
        => (pushText "\t"; continue());
<DOUBLEQUOTE>{eol}
        => (YYBEGIN INITIAL; Tok.StringLiteral (text()));
<DOUBLEQUOTE>[^"\\\n\r]+
        => (pushText yytext; continue());

<INITIAL>"'"
        => (YYBEGIN SINGLEQUOTE; continue());
<SINGLEQUOTE>"'"
        => (YYBEGIN INITIAL; Tok.Terminal (text()));
<SINGLEQUOTE>"\\'"
        => (pushText "'"; continue());
<SINGLEQUOTE>"\\\\"
        => (pushText "\\"; continue());
<SINGLEQUOTE>{eol}
        => (YYBEGIN INITIAL; Tok.Terminal (text()));
<SINGLEQUOTE>[^'\\\n\r]+
        => (pushText yytext; continue());

<INITIAL>{digit}+
        => (Tok.Int (valOf (Int.fromString yytext)));

<INITIAL>"left"         => (Tok.AssocLeft);
<INITIAL>"right"        => (Tok.AssocRight);
<INITIAL>"none"         => (Tok.AssocNone);
<INITIAL>"name"         => (Tok.RuleName);
<INITIAL>"assoc"        => (Tok.Assoc);
<INITIAL>"prec"         => (Tok.Prec);

<INITIAL>{ident}
        => (Tok.Ident yytext);

.       => (continue());
