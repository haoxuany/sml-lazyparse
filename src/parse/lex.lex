%defs (

structure Tok = ParseTokens

val comLvl : int ref = ref 0
val comStart : int ref = ref 0

type lex_result = Tok.token

fun eof () = (
      if !comLvl > 0
        then ()
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s :: (!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

);

%let eol = ("\n" | "\013\n" | "\013");
%let ws = ("\009" | "\011" | "\012" | " " | {eol});
%let lc = [a-z];
%let uc = [A-Z];
%let alpha = ({lc} | {uc});
%let digit = [0-9];
%let idchars = ({alpha} | {digit} | "_");
%let ident = {alpha}{idchars}*;

%states STRING SQSTRING COM;

%name Lex;

<INITIAL>{ws}+
        => (skip());

<INITIAL>"//"[^\n\013]*
        => (skip());

<INITIAL>"/*"
        => (comLvl := 1; comStart := !yylineno; YYBEGIN COM; continue());

<COM>"/*"
        => (comLvl := !comLvl + 1; continue());
<COM>"*/"
        => (comLvl := !comLvl - 1;
            if !comLvl = 0
              then (YYBEGIN INITIAL; continue())
              else continue());
<COM>. | {eol}
        => (continue());

<INITIAL>"::="
        => (Tok.COLONCOLONEQ);

<INITIAL>"|"    => (Tok.BAR);
<INITIAL>"*"    => (Tok.STAR);
<INITIAL>"?"    => (Tok.QUERY);
<INITIAL>"("    => (Tok.LP);
<INITIAL>")"    => (Tok.RP);
<INITIAL>"["    => (Tok.LB);
<INITIAL>"]"    => (Tok.RB);
<INITIAL>","    => (Tok.COMMA);

<INITIAL>"\""
        => (YYBEGIN STRING; clrText(); continue());
<STRING>"\""
        => (YYBEGIN INITIAL; Tok.STRING (getText()));
<STRING>"\\\""
        => (addText "\""; continue());
<STRING>"\\\\"
        => (addText "\\"; continue());
<STRING>"\\n"
        => (addText "\n"; continue());
<STRING>"\\t"
        => (addText "\t"; continue());
<STRING>{eol}
        => (YYBEGIN INITIAL; Tok.STRING (getText()));
<STRING>[^"\\\n\013]+
        => (addText yytext; continue());

<INITIAL>"'"
        => (YYBEGIN SQSTRING; clrText(); continue());
<SQSTRING>"'"
        => (YYBEGIN INITIAL; Tok.TERMINAL (getText()));
<SQSTRING>"\\'"
        => (addText "'"; continue());
<SQSTRING>"\\\\"
        => (addText "\\"; continue());
<SQSTRING>{eol}
        => (YYBEGIN INITIAL; Tok.TERMINAL (getText()));
<SQSTRING>[a-z][a-z_]*
        => (addText yytext; continue());

<INITIAL>{digit}+
        => (Tok.INT (valOf (Int.fromString yytext)));

<INITIAL>"left"         => (Tok.ASSOC_LEFT);
<INITIAL>"right"        => (Tok.ASSOC_RIGHT);
<INITIAL>"none"         => (Tok.ASSOC_NONE);
<INITIAL>"name"         => (Tok.RULE_NAME);
<INITIAL>"assoc"        => (Tok.KW_ASSOC);
<INITIAL>"prec"         => (Tok.KW_PREC);

<INITIAL>{ident}
        => (Tok.IDENT yytext);

.       => (continue());
