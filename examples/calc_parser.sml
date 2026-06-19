
structure CalcParser = struct
  open Stream

  structure Calc = Calc (
    val table_size = 256
    structure Stream = Stream
    structure Trivial = struct
      type t = unit
      type 'a stream = 'a stream

      fun lex (s , pos) =
        let
          fun advance (c , s' , pos) =
            case c of
              #"\n" => go (s' , Annot.newline 1 pos)
            | #"\r" =>
                (* peek for \r\n *)
                ( case front s' of
                    Cons (#"\n" , s'') => go (s'' , Annot.newline 2 pos)
                  | _ => go (s' , Annot.newline 1 pos)
                )
            | _ => go (s' , Annot.sameline 1 pos)
          and go (s , pos) =
            case front s of
              Cons (c , s') =>
                if Char.isSpace c
                then advance (c , s' , pos)
                else SOME (() , s , pos)
            | Nil => SOME (() , s , pos)
        in
          case front s of
            Cons (c , _) =>
              if Char.isSpace c
              then go (s , pos)
              else NONE
          | Nil => NONE
        end
    end
    structure Terminals = struct
      structure Number = struct
        type t = int
        type 'a stream = 'a stream

        fun lex (s , pos) =
          let
            fun go (acc , s , pos) =
              case front s of
                Cons (c , s') =>
                  if Char.isDigit c
                  then go (acc * 10 + (Char.ord c - Char.ord #"0") , s' , Annot.sameline 1 pos)
                  else SOME (acc , s , pos)
              | Nil => SOME (acc , s , pos)
          in
            case front s of
              Cons (c , _) =>
                if Char.isDigit c
                then go (0 , s , pos)
                else NONE
            | Nil => NONE
          end

        val show = Int.toString
      end
    end
  )

  open Calc

  fun run input =
    let
      val tokens = lex (fromString input) Annot.empty
      val results = parse parseStmt tokens
    in
      print (String.concat ["input: " , input , "\n"]);
      print (String.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (String.concat ["  " , printStmt result , "\n"]))
        results
    end
end
