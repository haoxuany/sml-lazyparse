
functor Repl (
  structure Result : sig
    type t
    type token_stream

    exception LexError of char * Annot.pos

    val lex : Char.char Stream.stream -> Annot.pos -> token_stream
    val parse : token_stream -> (t * token_stream) list
    val print : t -> string
  end
) = struct
  
  structure R = Result

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
              in
                print (String.concat
                  [ "parses (" , Int.toString (List.length results) , "): \n" ]
                );
                List.appi
                  (fn ( i , ( result , _ ) ) =>
                    print (String.concat [ Int.toString i , ". " , R.print result , "\n" ]))
                  results;
                loop ()
              end
              handle R.LexError (c , pos) =>
                let
                  val { lineno , colno , ... } = pos
                in
                  print (String.concat
                    [ "lex error: unexpected '"
                    , Char.toString c
                    , "' at "
                    , Int.toString lineno
                    , ":"
                    , Int.toString colno
                    , "\n"
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
