structure Codegen :
  sig
    val codegen : string -> Grammar.grammar -> unit
  end =
  struct

    structure G = Grammar
    structure NM = G.NameMap

    fun toSnakeCase s =
      let
        fun go (nil , _) = nil
          | go (c :: cs , isFirst) =
              if Char.isUpper c
              then if isFirst
                   then Char.toLower c :: go (cs , false)
                   else #"_" :: Char.toLower c :: go (cs , false)
              else c :: go (cs , false)
      in
        String.implode (go (String.explode s , true))
      end

    fun toPascalCase s =
      let
        fun go (nil , _) = nil
          | go (#"_" :: cs , _) = go (cs , true)
          | go (c :: cs , true) = Char.toUpper c :: go (cs , false)
          | go (c :: cs , false) = c :: go (cs , false)
      in
        String.implode (go (String.explode s , true))
      end

    fun nameOfId (m : G.id NM.dict) id =
      let
        fun search nil = raise Fail "unknown id"
          | search ((k , v) :: rest) =
              if v = id then k else search rest
      in
        search (NM.toList m)
      end

    fun ruleTypes nonterminalMap terminalMap rule =
      case rule
        of G.Seq rs => List.concatMap (ruleTypes nonterminalMap terminalMap) rs
         | G.Star r =>
             let val inner = ruleTypes nonterminalMap terminalMap r
             in case inner
                  of nil => nil
                   | [t] => [t ^ " list"]
                   | ts => [String.concat ["(" , String.concatWith " * " ts , ") list"]]
             end
         | G.Opt r =>
             let val inner = ruleTypes nonterminalMap terminalMap r
             in case inner
                  of nil => nil
                   | [t] => [t ^ " option"]
                   | ts => [String.concat ["(" , String.concatWith " * " ts , ") option"]]
             end
         | G.Terminal id => [String.concat ["Terminals." , toPascalCase (nameOfId terminalMap id) , ".t annot"]]
         | G.Keyword _ => nil
         | G.Nonterminal id => [toSnakeCase (nameOfId nonterminalMap id)]

    datatype form
      = FormNonfix
      | FormNonAssocInfix
      | FormLeftRecursive
      | FormRightRecursive

    fun classifyForm (alt : G.alt) =
      case #fixity alt
        of G.Nonfix => FormNonfix
         | G.Prefix => FormRightRecursive
         | G.Postfix => FormLeftRecursive
         | G.Infix assoc =>
             case assoc
               of G.Left => FormLeftRecursive
                | G.Right => FormRightRecursive
                | G.None => FormNonAssocInfix

    fun flattenRule r =
      case r
        of G.Seq rs => List.concatMap flattenRule rs
         | other => [other]

    structure IntMap = SplayDict (structure Key = IntOrdered)

    fun codegen (name : string) ({ nonterminalMap , terminalMap , definitions , keywords } : G.grammar) =
      let
        val filename = name ^ ".sml"
        val functorName = toPascalCase name
        val out = TextIO.openOut filename
        val pp = PrettyPrint.makeStream out 80
        open PrettyPrint

        val terminalNames = List.map #1 (NM.toList terminalMap)

        (* keyword string -> int id *)
        val keywordMap = List.mapi (fn (i , s) => (s , i)) keywords

        fun ntName id = nameOfId nonterminalMap id
        fun ntSnake id = toSnakeCase (ntName id)
        fun ntPascal id = toPascalCase (ntName id)
        fun tName id = nameOfId terminalMap id
        fun tPascal id = toPascalCase (tName id)

        val terminalWhere = "TERMINAL where type 'a stream = 'a Stream.stream"
        val terminalPrintableWhere = "TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream"

        fun emitFunctorHeader () =
          ( openBox pp Vertical 2
          ; print pp (String.concat ["functor " , functorName , " ("])
          ; break pp 0
          ; print pp "val table_size : int"
          ; break pp 0
          ; print pp "structure Stream : STREAM"
          ; break pp 0
          ; print pp "structure Trivial : "
          ; print pp terminalWhere
          ; break pp 0
          ; openBox pp Vertical 2
          ; print pp "structure Terminals : sig"
          ; List.app
              (fn name =>
                (break pp 0; print pp (String.concat ["structure " , toPascalCase name , " : " , terminalPrintableWhere])))
              terminalNames
          ; closeBox pp
          ; break pp 0
          ; print pp "end"
          ; closeBox pp
          ; break pp 0
          ; print pp ") :>"
          ; break pp 0
          )

        fun emitConstructor defName ({ rule , ruleName , ... } : G.alt) =
          let
            val conName = toPascalCase defName ^ ruleName
            val types = ruleTypes nonterminalMap terminalMap rule
          in
            case types
              of nil => conName
               | _ => String.concat [conName , " of " , String.concatWith " * " types]
          end

        fun emitDatatype isFirst { name = id , alts } =
          let
            val defName = ntName id
            val keyword = if isFirst then "datatype" else "and"
            val cons = List.map (emitConstructor defName) alts
          in
            case cons
              of nil => ()
               | first :: rest =>
                   ( openBox pp Vertical 2
                   ; print pp (String.concat [keyword , " " , ntSnake id , "' = " , first])
                   ; List.app (fn c => (break pp 0; print pp (String.concat ["| " , c]))) rest
                   ; closeBox pp
                   ; break pp 0
                   )
          end

        fun emitWithtype isFirst ({ name = id , ... } : G.definition) =
          let
            val keyword = if isFirst then "withtype" else "and"
          in
            print pp (String.concat [keyword , " " , ntSnake id , " = " , ntSnake id , "' annot"]);
            break pp 0
          end

        fun emitSealSig () =
          ( openBox pp Vertical 2
          ; print pp "sig"
          ; break pp 0
          ; print pp "type 'a annot = { node : 'a , span : Annot.span }"
          ; break pp 0
          ; List.appi
              (fn (i , { name = id , alts }) =>
                let
                  val keyword = if i = 0 then "datatype" else "and"
                  val cons = List.map (emitConstructor (ntName id)) alts
                in
                  case cons of
                    nil => ()
                  | first :: rest =>
                      ( print pp (String.concat [keyword , " " , ntSnake id , "' = " , first])
                      ; List.app (fn c => (break pp 0; print pp (String.concat ["| " , c]))) rest
                      ; break pp 0
                      )
                end)
              definitions
          ; List.appi
              (fn (i , { name = id , ... } : G.definition) =>
                let val keyword = if i = 0 then "withtype" else "and"
                in
                  print pp (String.concat [keyword , " " , ntSnake id , " = " , ntSnake id , "' annot"]);
                  break pp 0
                end)
              definitions
          ; break pp 0
          ; List.app
              (fn { name = id , ... } : G.definition =>
                ( print pp (String.concat ["val print" , ntPascal id , " : " , ntSnake id , " -> string"])
                ; break pp 0
                ))
              definitions
          ; break pp 0
          ; print pp "type 'a parser"
          ; break pp 0
          ; print pp "type token_stream"
          ; break pp 0
          ; print pp "val lex : char Stream.stream -> Annot.pos -> token_stream"
          ; break pp 0
          ; List.app
              (fn { name = id , ... } : G.definition =>
                ( print pp (String.concat ["val parse" , ntPascal id , " : " , ntSnake id , " parser"])
                ; break pp 0
                ))
              definitions
          ; print pp "val parse : 'a parser -> token_stream -> ('a * token_stream) list"
          ; closeBox pp
          ; break pp 0
          ; print pp "end ="
          )

        fun emitBackpatchRefs () =
          List.app
            (fn { name = id , ... } : G.definition =>
              ( print pp (String.concat
                  ["  val parse" , ntPascal id , "Dummy : " , ntSnake id , " t_dummy = dummy ()"])
              ; break pp 0
              ))
            definitions

        fun lookupKeyword s =
          case List.find (fn (s' , _) => s = s') keywordMap of
            SOME (_ , id) => id
          | NONE => raise Fail ("unknown keyword: " ^ s)

        (* kind: "kw" = keyword (span only), "v" = value, "skip" = no span or value *)
        (* needsSkip: whether to prefix with optional skipTrivial *)
        fun elemCode defId selfRef elem =
          case elem
            of G.Keyword s =>
                 { kind = "kw" , code = String.concat ["(keyword " , Int.toString (lookupKeyword s) , ")"] , needsSkip = true }
             | G.Terminal tid =>
                 { kind = "v" , code = String.concat ["(parseTerminal" , tPascal tid , ")"] , needsSkip = true }
             | G.Nonterminal nid =>
                 if nid = defId
                 then { kind = "v" , code = selfRef , needsSkip = false }
                 else { kind = "v" , code = String.concat ["(deref parse" , ntPascal nid , "Dummy)"] , needsSkip = false }
             | _ => { kind = "skip" , code = "(return ())" , needsSkip = false }

        (* extract span from a bound variable depending on its kind *)
        fun spanOf (kind , vname) =
          case kind of
            "kw" => vname
          | "v" => String.concat ["(#span " , vname , ")"]
          | _ => raise Fail "spanOf on skip"

        type bind = { kind : string , vname : string , code : string , needsSkip : bool }

        fun emitBindChain indent (binds : bind list) conName valueVars =
          let
            (* find first and last non-skip elements for span *)
            val spanElems = List.filter (fn { kind , ... } => kind <> "skip") binds
            val firstSpan =
              case spanElems of
                nil => NONE
              | { kind , vname , ... } :: _ => SOME (spanOf (kind , vname))
            val lastSpan =
              case spanElems of
                nil => NONE
              | _ => let val { kind , vname , ... } = List.last spanElems
                     in SOME (spanOf (kind , vname))
                     end

            val spanStr =
              case (firstSpan , lastSpan) of
                (SOME f , SOME l) =>
                  String.concat ["Annot.span (#start " , f , ") (#finish " , l , ")"]
              | _ => "Annot.span Annot.empty Annot.empty"

            val conStr =
              case valueVars
                of nil => conName
                 | [v] => String.concat [conName , " " , v]
                 | _ => String.concat [conName , " (" , String.concatWith " , " valueVars , ")"]

            val numCloses =
              List.foldl
                (fn ({ needsSkip , ... } , n) => n + 1 + (if needsSkip then 1 else 0))
                0 binds
              - 1

            fun emitReturn () =
              ( print pp (String.concat [indent , "return"])
              ; newline pp
              ; print pp (String.concat [indent , "  { node = " , conStr])
              ; newline pp
              ; print pp (String.concat [indent , "  , span = " , spanStr])
              ; newline pp
              ; print pp (String.concat [indent , "  }"])
              )

            fun emitBind vname code isLast =
              if isLast
              then
                ( print pp (String.concat [indent , "bind " , code , " (fn " , vname , " =>"])
                ; newline pp
                ; emitReturn ()
                ; print pp (String.implode (List.tabulate (numCloses , fn _ => #")"))
                            ^ ")")
                )
              else
                ( print pp (String.concat [indent , "bind " , code , " (fn " , vname , " =>"])
                ; newline pp
                )

            fun go (nil : bind list) = ()
              | go [{ vname , code , needsSkip , kind = _ }] =
                  ( if needsSkip
                    then ( print pp (String.concat [indent , "bind skipTrivial (fn _ =>"])
                         ; newline pp
                         )
                    else ()
                  ; emitBind vname code true
                  )
              | go ({ vname , code , needsSkip , kind = _ } :: rest) =
                  ( if needsSkip
                    then ( print pp (String.concat [indent , "bind skipTrivial (fn _ =>"])
                         ; newline pp
                         )
                    else ()
                  ; emitBind vname code false
                  ; go rest
                  )
          in
            case binds
              of nil => emitReturn ()
               | _ => go binds
          end

        fun emitRuleParser indent defId selfRef (alt : G.alt) =
          let
            val conName = String.concat [ntPascal defId , #ruleName alt]
            val elems = flattenRule (#rule alt)
            val (binds , valueVars , idx) =
              List.foldl
                (fn (elem , (bs , vs , idx)) =>
                  let
                    val { kind , code , needsSkip } = elemCode defId selfRef elem
                    val vname =
                      case kind of
                        "skip" => "_"
                      | _ => String.concat ["v" , Int.toString idx]
                    val nextIdx =
                      case kind of
                        "skip" => idx
                      | _ => idx + 1
                    val nextVs =
                      case kind of
                        "v" => vs @ [vname]
                      | _ => vs
                  in
                    (bs @ [{ kind = kind , vname = vname , code = code , needsSkip = needsSkip }] , nextVs , nextIdx)
                  end)
                (nil , nil , 0)
                elems
          in
            emitBindChain indent binds conName valueVars
          end

        fun emitParser indent { name = id , alts } =
          let
            val inner = indent ^ "  "
            val body = inner ^ "  "
            val bindIndent = body ^ "  "
            val selfRef = String.concat ["(deref parse" , ntPascal id , "Dummy)"]

            val nonfixAlts = List.filter (fn a => classifyForm a = FormNonfix) alts
            val fixityAlts = List.filter (fn a => classifyForm a <> FormNonfix) alts

            val precGroups : G.alt list IntMap.dict =
              List.foldl
                (fn (alt , m) =>
                  IntMap.insertMerge m (#precedence alt) [alt] (fn l => l @ [alt]))
                IntMap.empty
                fixityAlts

            val precList = List.rev (IntMap.toList precGroups)
          in
            print pp (String.concat [indent , "val parse" , ntPascal id , " = memoize"]);
            newline pp;
            print pp (String.concat [inner , "let"]);
            newline pp;

            List.appi
              (fn (i , alt) =>
                ( if i > 0 then newline pp else ()
                ; print pp (String.concat [body , "val parse" , #ruleName alt , " ="])
                ; newline pp
                ; emitRuleParser bindIndent id selfRef alt
                ; newline pp
                ))
              nonfixAlts;

            if List.null nonfixAlts then ()
            else newline pp;

            print pp (String.concat [body , "val parseAtom = either"]);
            newline pp;
            List.appi
              (fn (i , alt) =>
                let val prefix = if i = 0 then "[ " else ", "
                in
                  print pp (String.concat [body , prefix , "parse" , #ruleName alt]);
                  newline pp
                end)
              nonfixAlts;
            print pp (String.concat [body , "]"]);
            newline pp;
            newline pp;

            List.appi
              (fn (i , (prec , group)) =>
                let
                  val levelName = String.concat ["parseLevel" , Int.toString prec]
                  val higherName = if i = 0 then "parseAtom"
                                  else String.concat ["parseLevel" , Int.toString (#1 (List.nth (precList , i - 1)))]
                in
                  List.app
                    (fn alt =>
                      ( print pp (String.concat [body , "val parse" , #ruleName alt , " ="])
                      ; newline pp
                      ; emitRuleParser bindIndent id higherName alt
                      ; newline pp
                      ; newline pp
                      ))
                    group;

                  print pp (String.concat [body , "val " , levelName , " = either"]);
                  newline pp;
                  print pp (String.concat [body , "[ " , higherName]);
                  newline pp;
                  List.app
                    (fn alt =>
                      ( print pp (String.concat [body , ", parse" , #ruleName alt])
                      ; newline pp
                      ))
                    group;
                  print pp (String.concat [body , "]"]);
                  newline pp;
                  newline pp
                end)
              precList;

            print pp (String.concat [inner , "in"]);
            newline pp;
            ( case precList
                of nil => print pp (String.concat [body , "parseAtom"])
                 | _ =>
                     let val lastPrec = #1 (List.last precList)
                     in print pp (String.concat [body , "parseLevel" , Int.toString lastPrec])
                     end
            );
            newline pp;
            print pp (String.concat [inner , "end"]);
            newline pp
          end

        fun emitBackpatch () =
          List.app
            (fn { name = id , ... } : G.definition =>
              ( print pp (String.concat
                  ["val () = set parse" , ntPascal id , "Dummy parse" , ntPascal id])
              ; break pp 0
              ))
            definitions

        fun emitPrintElem indent buf elem idx =
          case elem of
            G.Keyword s =>
              ( print pp (String.concat
                  [indent , "PrintBuffer.push " , buf , " \"" , String.toString s , "\" 0"])
              ; NONE
              )
          | G.Terminal tid =>
              let val vname = String.concat ["v" , Int.toString idx]
              in
                print pp (String.concat
                  [indent , "let val { node = " , vname , "_node , span = { start = { lineno = " , vname , "_line , ... } , ... } } = " , vname]);
                newline pp;
                print pp (String.concat
                  [indent , "in PrintBuffer.push " , buf , " (Terminals." , tPascal tid , ".show " , vname , "_node) " , vname , "_line end"]);
                SOME (idx + 1)
              end
          | G.Nonterminal nid =>
              let val vname = String.concat ["v" , Int.toString idx]
              in
                print pp (String.concat
                  [indent , "print" , ntPascal nid , " " , buf , " " , vname]);
                SOME (idx + 1)
              end
          | _ => NONE

        fun emitPrintAlt indent defName (alt : G.alt) =
          let
            val elems = flattenRule (#rule alt)
            val conName = toPascalCase defName ^ (#ruleName alt)
            val valueElems =
              List.filter
                (fn G.Terminal _ => true | G.Nonterminal _ => true | _ => false)
                elems
            val numValues = List.length valueElems
            val patVars =
              case numValues of
                0 => ""
              | 1 => " v0"
              | n => String.concat [" (" , String.concatWith " , "
                  (List.tabulate (n , fn i => String.concat ["v" , Int.toString i])) , ")"]
          in
            print pp (String.concat [conName , patVars , " =>"]);
            newline pp;
            let
              val body = indent ^ "  "
              val needsParens = List.length elems > 1
            in
              if needsParens then (print pp (String.concat [body , "("]); newline pp) else ();
              List.foldl
                (fn (elem , (idx , first)) =>
                  let
                    val () = if first then () else (print pp ";"; newline pp)
                    val result = emitPrintElem body "buf" elem idx
                  in
                    case result of
                      SOME idx => (idx , false)
                    | NONE => (idx , false)
                  end)
                (0 , true)
                elems;
              if needsParens then (newline pp; print pp (String.concat [body , ")"])) else ();
              ()
            end
          end

        fun emitPrinter indent isFirst { name = id , alts } =
          let
            val defName = ntName id
            val inner = indent ^ "  "
            val body = inner ^ "  "
            val keyword = if isFirst then "fun" else "and"
          in
            print pp (String.concat
              [indent , keyword , " print" , ntPascal id , " buf ({ node , span = _ } : " , ntSnake id , ") ="]);
            newline pp;
            print pp (String.concat [inner , "case node of"]);
            newline pp;
            List.appi
              (fn (i , alt) =>
                ( if i > 0
                  then (newline pp; print pp (String.concat [body , "| "]))
                  else print pp (String.concat [body])
                ; emitPrintAlt body defName alt
                ))
              alts;
            newline pp
          end
      in
        openBox pp Vertical 0;
          emitFunctorHeader ();
          break pp 0;
          emitSealSig ();
          break pp 0;
          print pp "struct";
          break pp 0;
          openBox pp Vertical 2;
            break pp 0;
            print pp "type 'a annot = { node : 'a , span : Annot.span }";
            break pp 0;
            break pp 0;
            List.appi
              (fn (i , def) => emitDatatype (i = 0) def)
              definitions;
            List.appi
              (fn (i , def) => emitWithtype (i = 0) def)
              definitions;
            break pp 0;
            print pp "local";
            break pp 0;
            print pp "  structure LexInternal = LexInternal (structure Stream = Stream)";
            break pp 0;
            (* emit terminal_token datatype *)
            ( case terminalNames of
                nil => ()
              | first :: rest =>
                  ( print pp (String.concat
                      ["  datatype terminal_token = Terminal" , toPascalCase first , " of Terminals." , toPascalCase first , ".t"])
                  ; List.app
                      (fn name =>
                        ( break pp 0
                        ; print pp (String.concat
                            ["  | Terminal" , toPascalCase name , " of Terminals." , toPascalCase name , ".t"])
                        ))
                      rest
                  ; break pp 0
                  )
            );
            (* emit keywords list *)
            print pp "  val keywords =";
            break pp 0;
            ( case keywordMap of
                nil => print pp "    []"
              | (first :: rest) =>
                  ( print pp (String.concat
                      ["    [ (\"" , String.toString (#1 first) , "\" , " , Int.toString (#2 first) , ")"])
                  ; List.app
                      (fn (s , id) =>
                        ( break pp 0
                        ; print pp (String.concat
                            ["    , (\"" , String.toString s , "\" , " , Int.toString id , ")"])
                        ))
                      rest
                  ; break pp 0
                  ; print pp "    ]"
                  )
            );
            break pp 0;
            print pp "  structure Parcom = Parcom (";
            break pp 0;
            print pp "    type token = (int , Trivial.t , terminal_token) LexInternal.token";
            break pp 0;
            print pp "    val table_size = table_size";
            break pp 0;
            print pp "    structure Stream = Stream";
            break pp 0;
            print pp "  )";
            break pp 0;
            print pp "  open Parcom";
            break pp 0;
            print pp "  fun keyword k = terminal (fn";
            break pp 0;
            print pp "    LexInternal.TokenKeyword (k' , sp) => if k = k' then SOME sp else NONE";
            break pp 0;
            print pp "  | _ => NONE)";
            break pp 0;
            print pp "  val skipTrivial = optional (remove (fn";
            break pp 0;
            print pp "    LexInternal.TokenTrivial _ => true";
            break pp 0;
            print pp "  | _ => false))";
            break pp 0;
            print pp "  fun tag lex con (s , pos) =";
            break pp 0;
            print pp "    case lex (s , pos) of SOME (v , s' , pos') => SOME (con v , Annot.span pos pos' , s' , pos') | NONE => NONE";
            break pp 0;
            (* emit parseTerminal and per-terminal parsers *)
            print pp "  fun parseTerminal proj = terminal (fn";
            break pp 0;
            print pp "    LexInternal.TokenOther (v , sp) => (case proj v of SOME t => SOME { node = t , span = sp } | NONE => NONE)";
            break pp 0;
            print pp "  | _ => NONE)";
            break pp 0;
            List.app
              (fn name =>
                let
                  val pascal = toPascalCase name
                  val proj =
                    if List.length terminalNames = 1
                    then String.concat ["(fn Terminal" , pascal , " v => SOME v)"]
                    else String.concat ["(fn Terminal" , pascal , " v => SOME v | _ => NONE)"]
                in
                  print pp (String.concat
                    ["  val parseTerminal" , pascal , " = parseTerminal " , proj]);
                  break pp 0
                end)
              terminalNames;
            emitBackpatchRefs ();
            break pp 0;
            print pp "in";
            break pp 0;
            print pp "type 'a parser = 'a t_memo";
            break pp 0;
            print pp "type token_stream = (int , Trivial.t , terminal_token) LexInternal.token Stream.stream";
            break pp 0;
            break pp 0;
            (* emit lex function with terminal lexers *)
            ( case terminalNames of
                nil =>
                  print pp "fun lex s pos = LexInternal.lex s pos keywords Trivial.lex []"
              | _ =>
                  ( print pp "fun lex s pos = LexInternal.lex s pos keywords Trivial.lex"
                  ; break pp 0
                  ; List.appi
                      (fn (i , name) =>
                        let
                          val pascal = toPascalCase name
                          val prefix = if i = 0 then "  [ " else "  , "
                        in
                          print pp (String.concat
                            [prefix , "tag Terminals." , pascal , ".lex Terminal" , pascal]);
                          break pp 0
                        end)
                      terminalNames
                  ; print pp "  ]"
                  )
            );
            break pp 0;
            break pp 0;
            List.app (emitParser "  ") definitions;
            break pp 0;
            emitBackpatch ();
            break pp 0;
            List.appi (fn (i , def) => emitPrinter "  " (i = 0) def) definitions;
            break pp 0;
            (* shadow print functions to hide PrintBuffer *)
            print pp "fun print f v = let val buf = PrintBuffer.empty () in f buf v; PrintBuffer.toString buf end";
            break pp 0;
            List.app
              (fn { name = id , ... } : G.definition =>
                ( print pp (String.concat
                    ["val print" , ntPascal id , " = print print" , ntPascal id])
                ; break pp 0
                ))
              definitions;
            break pp 0;
            print pp "val parse = parser";
            break pp 0;
            break pp 0;
            print pp "end";
          closeBox pp;
          break pp 0;
          break pp 0;
          print pp "end";
        closeBox pp;
        flush pp;
        TextIO.closeOut out
      end

  end
