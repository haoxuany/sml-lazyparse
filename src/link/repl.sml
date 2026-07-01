
functor Repl (
  structure Result : sig
    type t

    structure TokenStream : sig
      type t
      type token

      datatype front = Nil | Cons of token * t

      val front : t -> front
    end

    datatype 'a result =
      Success of ('a * TokenStream.t) list
    | Fail of ParseError.t

    exception LexError of LexStream.stream

    val lex : Char.char Stream.stream -> Annot.pos -> TokenStream.t
    val parse : TokenStream.t -> t result
    val print : t -> string
  end
) = struct

  structure R = Result
  structure LS = LexStream

  fun run () =
    let
      val prompt = "> "
      val continuation = "...  "

      fun readLines acc =
        case TextIO.inputLine TextIO.stdIn of
          NONE => NONE
        | SOME line =>
            let
              val trimmed = Substring.string
                (Substring.dropr Char.isSpace (Substring.full line))
              val len = String.size trimmed
            in
              if len >= 2
                andalso String.sub ( trimmed , len - 1 ) = #"\\"
                andalso String.sub ( trimmed , len - 2 ) = #"\\"
              then
                SOME (List.rev (String.substring ( trimmed , 0 , len - 1 ) :: acc))
              else if len >= 1
                andalso String.sub ( trimmed , len - 1 ) = #"\\"
              then
                ( print continuation
                ; readLines (String.substring ( trimmed , 0 , len - 1 ) :: acc)
                )
              else
                SOME (List.rev (trimmed :: acc))
            end


      fun loop () =
        ( print prompt
        ; case readLines nil of
            NONE => ()
          | SOME input =>
              let
                val input = String.concatWith "\n" input

                val tokens = R.lex (Stream.fromString input) Annot.empty
                val results = R.parse tokens

                fun remaining s n =
                  case R.TokenStream.front s of
                    R.TokenStream.Nil => n
                  | R.TokenStream.Cons ( _ , s ) => remaining s (n + 1)

              in
                case results of
                  R.Success parses =>
                    ( print (String.concat
                        [ "parses (" , Int.toString (List.length parses) , "): \n" ])
                    ; List.appi
                        (fn ( i , ( result , trailing ) ) =>
                          let
                            val rem = remaining trailing 0
                            val trailing =
                              if rem = 0 then ""
                              else String.concat
                                [ " (" , Int.toString rem , " trailing)" ]
                          in
                            print (String.concat
                              [ Int.toString i , ". " , R.print result
                              , trailing , "\n" ])
                          end)
                        parses
                    )
                | R.Fail error =>
                    ( print "parse error:\n"
                    ; print (String.concat [ "  " , ParseError.show error , "\n" ])
                    );
                loop ()
              end
              handle R.LexError s =>
                let
                  val { lineno , colno , ... } = LS.pos s

                  fun restOfLine s acc =
                    case LS.front s of
                      LS.Nil => String.implode (List.rev acc)
                    | LS.Cons ( #"\n" , _ ) => String.implode (List.rev acc)
                    | LS.Cons ( #"\r" , _ ) => String.implode (List.rev acc)
                    | LS.Cons ( c , s ) => restOfLine s (c :: acc)

                  val rest = restOfLine s nil
                in
                  print (String.concat
                    [ "lex error at "
                    , Int.toString lineno
                    , ":"
                    , Int.toString colno
                    , ": '"
                    , rest
                    , "'\n"
                    ]);
                  loop ()
                end
        )
    in
      print
      "Use \\ to continue string in new line at EOL and \\\\ for the literal backslash at EOL" ;
      print "\n";
      loop ()
    end
end
