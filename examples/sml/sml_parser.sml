
structure SmlParser = struct
  structure LS = LexStream
  structure R = Regex

  structure C = Char
  structure S = String

  (* SML symbolic characters *)
  val symbolic = R.set
    [ #"!" , #"%" , #"&" , #"$"
    , #"#" , #"+" , #"-" , #"/"
    , #":" , #"<" , #"=" , #">"
    , #"?" , #"@" , #"\\" , #"~"
    , #"`" , #"^" , #"|" , #"*"
    ]

  val idChar = R.alt
    [ R.alphaNum
    , R.exact #"'"
    , R.exact #"_"
    ]

  (* lex an SML char literal (after #") *)
  fun lexCharBody ts =
    case LS.front ts of
      LS.Nil => NONE
    | LS.Cons (#"\\" , ts') =>
        ( case LS.front ts' of
            LS.Nil => NONE
          | LS.Cons (c , ts'') =>
              ( case LS.front ts'' of
                  LS.Cons (#"\"" , ts''') =>
                    SOME (S.implode [#"\\", c] , ts''')
                | _ => NONE
              )
        )
    | LS.Cons (c , ts') =>
        ( case LS.front ts' of
            LS.Cons (#"\"" , ts'') =>
              SOME (S.str c , ts'')
          | _ => NONE
        )

  structure Terminals = struct
    structure Int = LexCommon.RegexReplTerminal (
      type t = string
      val regex = R.seq [R.opt (R.exact #"~") , R.plus R.digit]
      fun map s = s
      fun show s = s
    )

    structure Word = LexCommon.RegexReplTerminal (
      type t = string
      val regex = R.seq [R.exact #"0" , R.exact #"w" , R.plus R.digit]
      fun map s = s
      fun show s = s
    )

    structure Float = LexCommon.RegexReplTerminal (
      type t = string
      val regex = R.seq
        [ R.opt (R.exact #"~")
        , R.plus R.digit
        , R.exact #"."
        , R.plus R.digit
        ]
      fun map s = s
      fun show s = s
    )

    (* Char literal: #"c" *)
    structure Char = struct
      type t = string
      fun lex ts =
        case LS.front ts of
          LS.Cons (#"#" , ts') =>
            ( case LS.front ts' of
                LS.Cons (#"\"" , ts'') =>
                  ( case lexCharBody ts'' of
                      SOME (c , ts''') => SOME (c , ts''')
                    | NONE => NONE
                  )
              | _ => NONE
            )
        | _ => NONE
      val show = fn c => S.concat ["#\"" , c , "\""]
    end

    structure String = LexCommon.StringTerminal

    structure Id = LexCommon.RegexReplTerminal (
      type t = string
      val regex = R.alt
        [ R.seq [R.alpha , R.star idChar]
        , R.plus symbolic
        ]
      fun map s = s
      fun show s = s
    )

    structure Tyvar = LexCommon.RegexReplTerminal (
      type t = string
      val regex = R.seq [R.exact #"'" , R.star idChar]
      fun map s = s
      fun show s = s
    )

  end

  structure Sml = SmlParser (
    structure Trivial = struct
      type t = unit

      fun lex ts =
        let
          fun skipBlockComment (depth , ts) =
            case LS.front ts of
              LS.Nil => ts
            | LS.Cons (#"(" , ts') =>
                ( case LS.front ts' of
                    LS.Cons (#"*" , ts'') =>
                      skipBlockComment (depth + 1 , ts'')
                  | _ => skipBlockComment (depth , ts')
                )
            | LS.Cons (#"*" , ts') =>
                ( case LS.front ts' of
                    LS.Cons (#")" , ts'') =>
                      if depth = 1 then ts''
                      else skipBlockComment (depth - 1 , ts'')
                  | _ => skipBlockComment (depth , ts')
                )
            | LS.Cons (_ , ts') =>
                skipBlockComment (depth , ts')

          fun skip (ts , consumed) =
            case LS.front ts of
              LS.Cons (c , ts') =>
                if C.isSpace c
                then skip (ts' , true)
                else if c = #"(" then
                  ( case LS.front ts' of
                      LS.Cons (#"*" , ts'') =>
                        let val ts''' = skipBlockComment (1 , ts'')
                        in skip (ts''' , true)
                        end
                    | _ =>
                        if consumed then SOME (() , ts) else NONE
                  )
                else
                  if consumed then SOME (() , ts) else NONE
            | LS.Nil =>
                if consumed then SOME (() , ts) else NONE
        in
          skip (ts , false)
        end
    end
    structure Terminals = Terminals
  )

  structure Print = SmlPrint (
    structure Ast = Sml
    structure Terminals = Terminals
  )

  open Sml

  fun run input =
    let
      val tokens = lex (Stream.fromString input) Annot.empty
      val results = parse parseDecList tokens
    in
      print (S.concat ["input: " , input , "\n"]);
      print (S.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (S.concat ["  " , Print.printDecList result , "\n"]))
        results
    end

  fun runExp input =
    let
      val tokens = lex (Stream.fromString input) Annot.empty
      val results = parse parseExp tokens
    in
      print (S.concat ["input: " , input , "\n"]);
      print (S.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (S.concat ["  " , Print.printExp result , "\n"]))
        results
    end

  fun runProg input =
    let
      val tokens = lex (Stream.fromString input) Annot.empty
      val results = parse parseProgList tokens
    in
      print (S.concat ["input: " , input , "\n"]);
      print (S.concat ["parses: " , Int.toString (List.length results) , "\n"]);
      List.app
        (fn (result , _) =>
          print (S.concat ["  " , Print.printProgList result , "\n"]))
        results
    end
end
