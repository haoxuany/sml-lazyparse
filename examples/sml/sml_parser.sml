
structure SmlParser = struct
  open Stream

  structure C = Char
  structure S = String

  (* SML symbolic characters *)
  fun isSymbolic c =
    case c of
      #"!" => true | #"%" => true | #"&" => true | #"$" => true
    | #"#" => true | #"+" => true | #"-" => true | #"/" => true
    | #":" => true | #"<" => true | #"=" => true | #">" => true
    | #"?" => true | #"@" => true | #"\\" => true | #"~" => true
    | #"`" => true | #"^" => true | #"|" => true | #"*" => true
    | _ => false

  fun isIdChar c =
    C.isAlphaNum c orelse c = #"'" orelse c = #"_"

  (* advance position helpers *)
  fun adv (s , pos) =
    case front s of
      Cons (c , s') =>
        if c = #"\n" then (s' , Annot.newline 1 pos)
        else (s' , Annot.sameline 1 pos)
    | Nil => (s , pos)

  (* consume while predicate holds, return string and new stream/pos *)
  fun takeWhile pred (s , pos) =
    let
      fun go (acc , s , pos) =
        case front s of
          Cons (c , s') =>
            if pred c
            then go (c :: acc , s' , Annot.sameline 1 pos)
            else (S.implode (List.rev acc) , s , pos)
        | Nil => (S.implode (List.rev acc) , s , pos)
    in
      go (nil , s , pos)
    end

  (* lex an SML string literal (after opening ") *)
  fun lexStringBody (s , pos) =
    let
      fun go (acc , s , pos) =
        case front s of
          Nil => NONE
        | Cons (#"\"" , s') =>
            SOME (S.implode (List.rev acc) , s' , Annot.sameline 1 pos)
        | Cons (#"\\" , s') =>
            ( case front s' of
                Nil => NONE
              | Cons (c , s'') =>
                  go (c :: #"\\" :: acc , s'' , Annot.sameline 2 pos)
            )
        | Cons (c , s') =>
            go (c :: acc , s' , Annot.sameline 1 pos)
    in
      go (nil , s , pos)
    end

  (* lex an SML char literal (after #") *)
  fun lexCharBody (s , pos) =
    case front s of
      Nil => NONE
    | Cons (#"\\" , s') =>
        ( case front s' of
            Nil => NONE
          | Cons (c , s'') =>
              ( case front s'' of
                  Cons (#"\"" , s''') =>
                    SOME (S.implode [#"\\", c] , s''' , Annot.sameline 3 pos)
                | _ => NONE
              )
        )
    | Cons (c , s') =>
        ( case front s' of
            Cons (#"\"" , s'') =>
              SOME (S.str c , s'' , Annot.sameline 2 pos)
          | _ => NONE
        )

  structure Sml = Sml (
    val table_size = 1024
    structure Stream = Stream
    structure Trivial = struct
      type t = unit
      type 'a stream = 'a stream

      fun lex (s , pos) =
        let
          fun skipLineComment (s , pos) =
            case front s of
              Nil => (s , pos)
            | Cons (#"\n" , s') => (s' , Annot.newline 1 pos)
            | Cons (_ , s') => skipLineComment (s' , Annot.sameline 1 pos)

          fun skipBlockComment (depth , s , pos) =
            case front s of
              Nil => (s , pos)
            | Cons (#"(" , s') =>
                ( case front s' of
                    Cons (#"*" , s'') =>
                      skipBlockComment (depth + 1 , s'' , Annot.sameline 2 pos)
                  | _ => skipBlockComment (depth , s' , Annot.sameline 1 pos)
                )
            | Cons (#"*" , s') =>
                ( case front s' of
                    Cons (#")" , s'') =>
                      if depth = 1 then (s'' , Annot.sameline 2 pos)
                      else skipBlockComment (depth - 1 , s'' , Annot.sameline 2 pos)
                  | _ => skipBlockComment (depth , s' , Annot.sameline 1 pos)
                )
            | Cons (#"\n" , s') =>
                skipBlockComment (depth , s' , Annot.newline 1 pos)
            | Cons (_ , s') =>
                skipBlockComment (depth , s' , Annot.sameline 1 pos)

          fun go (s , pos , consumed) =
            case front s of
              Cons (c , s') =>
                if C.isSpace c
                then
                  let val pos' =
                    if c = #"\n" then Annot.newline 1 pos
                    else Annot.sameline 1 pos
                  in go (s' , pos' , true)
                  end
                else if c = #"(" then
                  ( case front s' of
                      Cons (#"*" , s'') =>
                        let val (s''' , pos') = skipBlockComment (1 , s'' , Annot.sameline 2 pos)
                        in go (s''' , pos' , true)
                        end
                    | _ =>
                        if consumed then SOME (() , s , pos) else NONE
                  )
                else
                  if consumed then SOME (() , s , pos) else NONE
            | Nil =>
                if consumed then SOME (() , s , pos) else NONE
        in
          go (s , pos , false)
        end
    end
    structure Terminals = struct
      (* Integer literal *)
      structure Int = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (#"~" , s') =>
              let val (digits , s'' , pos') = takeWhile C.isDigit (s' , Annot.sameline 1 pos)
              in if S.size digits > 0
                 then SOME ("~" ^ digits , s'' , pos')
                 else NONE
              end
          | Cons (c , _) =>
              if C.isDigit c
              then let val (digits , s' , pos') = takeWhile C.isDigit (s , pos)
                   in SOME (digits , s' , pos')
                   end
              else NONE
          | Nil => NONE
        val show = fn s => s
      end

      (* Word literal *)
      structure Word = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (#"0" , s') =>
              ( case front s' of
                  Cons (#"w" , s'') =>
                    let val (digits , s''' , pos') = takeWhile C.isDigit (s'' , Annot.sameline 2 pos)
                    in if S.size digits > 0
                       then SOME ("0w" ^ digits , s''' , pos')
                       else NONE
                    end
                | _ => NONE
              )
          | _ => NONE
        val show = fn s => s
      end

      (* Float literal *)
      structure Float = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (#"~" , _) =>
              let val (whole , s' , pos') = takeWhile (fn c => c = #"~" orelse C.isDigit c) (s , pos)
              in
                case front s' of
                  Cons (#"." , s'') =>
                    let val (frac , s''' , pos'') = takeWhile C.isDigit (s'' , Annot.sameline 1 pos')
                    in if S.size frac > 0
                       then SOME (whole ^ "." ^ frac , s''' , pos'')
                       else NONE
                    end
                | _ => NONE
              end
          | Cons (c , _) =>
              if C.isDigit c
              then
                let val (whole , s' , pos') = takeWhile C.isDigit (s , pos)
                in
                  case front s' of
                    Cons (#"." , s'') =>
                      let val (frac , s''' , pos'') = takeWhile C.isDigit (s'' , Annot.sameline 1 pos')
                      in if S.size frac > 0
                         then SOME (whole ^ "." ^ frac , s''' , pos'')
                         else NONE
                      end
                  | _ => NONE
                end
              else NONE
          | Nil => NONE
        val show = fn s => s
      end

      (* Char literal: #"c" *)
      structure Char = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (#"#" , s') =>
              ( case front s' of
                  Cons (#"\"" , s'') =>
                    ( case lexCharBody (s'' , Annot.sameline 2 pos) of
                        SOME (c , s''' , pos') => SOME (c , s''' , pos')
                      | NONE => NONE
                    )
                | _ => NONE
              )
          | _ => NONE
        val show = fn c => S.concat ["#\"" , c , "\""]
      end

      (* String literal *)
      structure String = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (#"\"" , s') =>
              ( case lexStringBody (s' , Annot.sameline 1 pos) of
                  SOME (str , s'' , pos') => SOME (str , s'' , pos')
                | NONE => NONE
              )
          | _ => NONE
        val show = fn s => S.concat ["\"" , s , "\""]
      end

      (* Identifier (alphanumeric or symbolic) *)
      structure Id = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (c , _) =>
              if C.isAlpha c
              then let val (id , s' , pos') = takeWhile isIdChar (s , pos)
                   in SOME (id , s' , pos')
                   end
              else if isSymbolic c
              then let val (id , s' , pos') = takeWhile isSymbolic (s , pos)
                   in SOME (id , s' , pos')
                   end
              else NONE
          | Nil => NONE
        val show = fn s => s
      end

      (* Type variable: 'a, ''a *)
      structure Tyvar = struct
        type t = string
        type 'a stream = 'a stream
        fun lex (s , pos) =
          case front s of
            Cons (#"'" , s') =>
              let val (rest , s'' , pos') = takeWhile isIdChar (s' , Annot.sameline 1 pos)
              in SOME ("'" ^ rest , s'' , pos')
              end
          | _ => NONE
        val show = fn s => s
      end

    end
  )

  open Sml

  fun run input =
    let
      val tokens = lex (fromString input) Annot.empty
      val results = parse parseDecList tokens
    in
      print (S.concat ["input: " , input , "\n"]);
      print (S.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (S.concat ["  " , printDecList result , "\n"]))
        results
    end

  fun runExp input =
    let
      val tokens = lex (fromString input) Annot.empty
      val results = parse parseExp tokens
    in
      print (S.concat ["input: " , input , "\n"]);
      print (S.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (S.concat ["  " , printExp result , "\n"]))
        results
    end
end
