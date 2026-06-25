structure S = Stream

structure JsonParser = struct
  structure Json = JsonParser (
    structure Stream = S
    structure Trivial = struct
      type t = unit
      type 'a stream = 'a S.stream

      fun lex ( (s , pos) : char S.stream * Annot.pos )
        : (t * char S.stream * Annot.pos) option =
        case S.front s of
          S.Cons ( #" " , s' ) => SOME (() , s' , Annot.sameline 1 pos)
        | S.Cons ( #"\t" , s' ) => SOME (() , s' , Annot.sameline 1 pos)
        | S.Cons ( #"\n" , s' ) => SOME (() , s' , Annot.newline 1 pos)
        | S.Cons ( #"\r" , s' ) => SOME (() , s' , Annot.sameline 1 pos)
        | _ => NONE
    end
    structure Terminals = struct
      structure Number = struct
        type t = string
        type 'a stream = 'a S.stream

        fun lex ( (s , pos) : char S.stream * Annot.pos )
          : (t * char S.stream * Annot.pos) option =
          let
            fun isNumChar c = Char.isDigit c orelse c = #"." orelse c = #"-"
                              orelse c = #"+" orelse c = #"e" orelse c = #"E"
            fun go (acc , s , pos) =
              case S.front s of
                S.Cons ( c , s' ) =>
                  if isNumChar c
                  then go (c :: acc , s' , Annot.sameline 1 pos)
                  else (String.implode (List.rev acc) , s , pos)
              | S.Nil => (String.implode (List.rev acc) , s , pos)
          in
            case S.front s of
              S.Cons ( c , s' ) =>
                if Char.isDigit c orelse c = #"-"
                then let val (num , s'' , pos') = go ([c] , s' , Annot.sameline 1 pos)
                     in SOME (num , s'' , pos')
                     end
                else NONE
            | S.Nil => NONE
          end

        fun show t = t
      end

      structure String = struct
        type t = string
        type 'a stream = 'a S.stream

        fun lex ( (s , pos) : char S.stream * Annot.pos )
          : (t * char S.stream * Annot.pos) option =
          case S.front s of
            S.Cons ( #"\"" , s' ) =>
              let
                fun go (acc , s , pos) =
                  case S.front s of
                    S.Nil => NONE
                  | S.Cons ( #"\"" , s' ) =>
                      SOME (String.implode (List.rev acc) , s' , Annot.sameline 1 pos)
                  | S.Cons ( #"\\" , s' ) =>
                      ( case S.front s' of
                          S.Nil => NONE
                        | S.Cons ( c , s'' ) =>
                            go (c :: #"\\" :: acc , s'' , Annot.sameline 2 pos)
                      )
                  | S.Cons ( c , s' ) =>
                      go (c :: acc , s' , Annot.sameline 1 pos)
              in
                go (nil , s' , Annot.sameline 1 pos)
              end
          | _ => NONE

        fun show t = "\"" ^ t ^ "\""
      end
    end
  )

  open Json

  fun nodeOf ({ node , ... } : string annot) = node

  fun posStr (p : Annot.pos) =
    Int.toString (#lineno p) ^ ":" ^ Int.toString (#colno p)

  fun spanStr ({ start , finish } : Annot.span) =
    posStr start ^ "-" ^ posStr finish

  fun annot name f ({ node , span } : 'a annot) =
    name ^ "(" ^ f node ^ ") @" ^ spanStr span

  fun commas f xs = String.concatWith ", " (List.map f xs)
  fun opt f NONE = "null"
    | opt f (SOME x) = f x

  fun printValue x = annot "" (fn
      ValueString s => "\"" ^ nodeOf s ^ "\""
    | ValueNumber n => nodeOf n
    | ValueObject obj => printObject obj
    | ValueArray arr => printArray arr
    | ValueTrue => "true"
    | ValueFalse => "false"
    | ValueNull => "null"
    ) x

  and printObject x = annot "" (fn
      ObjectObject NONE => "{}"
    | ObjectObject (SOME (m , ms)) =>
        "{" ^ commas printMember (m :: ms) ^ "}"
    ) x

  and printMember x = annot "" (fn
      MemberMember (k , v) =>
        "\"" ^ nodeOf k ^ "\": " ^ printValue v
    ) x

  and printArray x = annot "" (fn
      ArrayArray NONE => "[]"
    | ArrayArray (SOME (v , vs)) =>
        "[" ^ commas printValue (v :: vs) ^ "]"
    ) x

  fun run input =
    let
      val tokens = lex (S.fromString input) Annot.empty
      val results = parse parseValue tokens
    in
      print (String.concat ["input: " , input , "\n"]);
      print (String.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (String.concat ["  " , printValue result , "\n"]))
        results
    end
end
