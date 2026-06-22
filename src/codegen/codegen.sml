structure Codegen :
  sig
    val codegen : string -> Grammar.grammar -> unit
  end =
  struct

    structure G = Grammar
    structure IM = G.IdMap

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

    fun nameOfId (m : string IM.dict) id =
      case IM.find m id of
        SOME name => name
      | NONE => raise Fail "unknown id"

    fun ruleTypes nonterminals terminals spec =
      let
        fun wrap suffix inner =
          case inner of
            nil => nil
          | [t] => [t ^ suffix]
          | ts => [String.concat ["(" , String.concatWith " * " ts , ")" , suffix]]
      in
        case spec of
          G.Seq rs => List.concatMap (ruleTypes nonterminals terminals) rs
        | G.Star r => wrap " list" (ruleTypes nonterminals terminals r)
        | G.Plus r => wrap " list" (ruleTypes nonterminals terminals r)
        | G.Opt r => wrap " option" (ruleTypes nonterminals terminals r)
        | G.Terminal id => [String.concat ["Terminals." , toPascalCase (nameOfId terminals id) , ".t annot"]]
        | G.Keyword _ => nil
        | G.Nonterminal id => [toSnakeCase (nameOfId nonterminals id)]
      end

    datatype form
      = FormNonfix
      | FormNonAssocInfix
      | FormLeftRecursive
      | FormRightRecursive

    fun classifyForm ({ fixity , ... } : G.rule) =
      case fixity of
        G.Nonfix => FormNonfix
      | G.Prefix => FormRightRecursive
      | G.Postfix => FormLeftRecursive
      | G.Infix assoc =>
          case assoc of
            G.Left => FormLeftRecursive
          | G.Right => FormRightRecursive
          | G.None => FormNonAssocInfix

    fun flattenSpec r =
      case r of
        G.Seq rs => List.concatMap flattenSpec rs
      | _ => [r]

    structure IntMap = SplayDict (structure Key = IntOrdered)

    fun codegen (name : string) ({ nonterminals , terminals , definitions , keywords } : G.grammar) =
      let
        val filename = name ^ ".sml"
        val functorName = toPascalCase name
        val out = TextIO.openOut filename
        val pp = PrettyPrint.makeStream out 80
        open PrettyPrint

        val terminalNames = List.map (fn (_ , name) => name) (IM.toList terminals)

        val keywordList = IM.toList keywords

        fun ntName id = nameOfId nonterminals id
        fun ntSnake id = toSnakeCase (ntName id)
        fun ntPascal id = toPascalCase (ntName id)
        fun tName id = nameOfId terminals id
        fun tPascal id = toPascalCase (tName id)
        fun kwName id = nameOfId keywords id

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

        fun emitConstructor defName ({ spec , name , ... } : G.rule) =
          let
            val conName = toPascalCase defName ^ name
            val types = ruleTypes nonterminals terminals spec
          in
            case types
              of nil => conName
               | _ => String.concat [conName , " of " , String.concatWith " * " types]
          end

        fun emitDatatype isFirst { name = id , rules } =
          let
            val defName = ntName id
            val keyword = if isFirst then "datatype" else "and"
            val cons = List.map (emitConstructor defName) rules
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
              (fn (i , { name = id , rules }) =>
                let
                  val keyword = if i = 0 then "datatype" else "and"
                  val cons = List.map (emitConstructor (ntName id)) rules
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

        fun isValueElem elem =
          case elem of
            G.Terminal _ => true
          | G.Nonterminal _ => true
          | G.Star r => isValueElem r
          | G.Plus r => isValueElem r
          | G.Opt r => isValueElem r
          | G.Seq rs => List.exists isValueElem rs
          | G.Keyword _ => false

        fun emitSubParser indent defId leftRef rightRef spec =
          let
            val elems = flattenSpec spec
            val valueCount = List.foldl (fn (e , n) => if isValueElem e then n + 1 else n) 0 elems

            fun emitInner indent elems idx =
              case elems of
                nil =>
                  let val retExpr =
                    case valueCount of
                      0 => "return ()"
                    | 1 => String.concat ["return v0"]
                    | _ => String.concat ["return (" , String.concatWith " , "
                        (List.tabulate (valueCount , fn i => String.concat ["v" , Int.toString i])) , ")"]
                  in
                    print pp (String.concat [indent , retExpr])
                  end
              | elem :: rest =>
                  let
                    val resolved = resolveElem defId leftRef rightRef elem
                  in
                    case resolved of
                      NONE => emitInner indent rest idx
                    | SOME { code , needsSkip , isValue } =>
                        let
                          val vname = if isValue
                                     then String.concat ["v" , Int.toString idx]
                                     else "_"
                          val nextIdx = if isValue then idx + 1 else idx
                        in
                          if needsSkip
                          then ( print pp (String.concat [indent , "bind skipTrivial (fn _ =>"])
                               ; newline pp
                               )
                          else ();
                          print pp (String.concat [indent , "bind " , code , " (fn " , vname , " =>"]);
                          newline pp;
                          emitInner indent rest nextIdx;
                          print pp ")";
                          if needsSkip then print pp ")" else ()
                        end
                  end
          in
            emitInner indent elems 0
          end

        and resolveElem defId leftRef rightRef elem =
          case elem of
            G.Keyword kid =>
              SOME { code = String.concat ["(keyword " , Int.toString kid , ")"]
                   , needsSkip = true , isValue = false }
          | G.Terminal tid =>
              SOME { code = String.concat ["(parseTerminal" , tPascal tid , ")"]
                   , needsSkip = true , isValue = true }
          | G.Nonterminal nid =>
              if nid = defId
              then SOME { code = leftRef , needsSkip = false , isValue = true }
              else SOME { code = String.concat ["(deref parse" , ntPascal nid , "Dummy)"]
                        , needsSkip = false , isValue = true }
          | G.Opt r =>
              SOME { code = String.concat ["(optionalLongest (" , subParserCode defId leftRef rightRef r , "))"]
                   , needsSkip = false , isValue = isValueElem r }
          | G.Star r =>
              SOME { code = String.concat ["(starLongest (" , subParserCode defId leftRef rightRef r , "))"]
                   , needsSkip = false , isValue = isValueElem r }
          | G.Plus r =>
              SOME { code = String.concat ["(plusLongest (" , subParserCode defId leftRef rightRef r , "))"]
                   , needsSkip = false , isValue = isValueElem r }
          | G.Seq _ => NONE

        and subParserCode defId leftRef rightRef spec =
          let
            val elems = flattenSpec spec
            val valueCount = List.foldl (fn (e , n) => if isValueElem e then n + 1 else n) 0 elems

            fun go (nil , _ , acc , closes) =
                  let val retExpr =
                    case valueCount of
                      0 => "return ()"
                    | 1 => "return v0"
                    | _ => String.concat ["return (" , String.concatWith " , "
                        (List.tabulate (valueCount , fn i => String.concat ["v" , Int.toString i])) , ")"]
                  in
                    List.rev (retExpr :: acc) @ [String.implode (List.tabulate (closes , fn _ => #")"))]
                  end
              | go (elem :: rest , idx , acc , closes) =
                  ( case resolveElem defId leftRef rightRef elem of
                      NONE => go (rest , idx , acc , closes)
                    | SOME { code , needsSkip , isValue } =>
                        let
                          val vname = if isValue
                                     then String.concat ["v" , Int.toString idx]
                                     else "_"
                          val nextIdx = if isValue then idx + 1 else idx
                          val prefix = if needsSkip
                                      then "bind skipTrivial (fn _ => "
                                      else ""
                          val extraCloses = if needsSkip then 2 else 1
                          val line = String.concat [prefix , "bind " , code , " (fn " , vname , " => "]
                        in
                          go (rest , nextIdx , line :: acc , closes + extraCloses)
                        end
                  )

            val parts = go (elems , 0 , nil , 0)
          in
            String.concat parts
          end

        fun elemCode defId leftRef rightRef elem =
          case resolveElem defId leftRef rightRef elem of
            NONE => { kind = "skip" , code = "(return ())" , needsSkip = false }
          | SOME { code , needsSkip , isValue } =>
              { kind = if isValue then "v" else "kw"
              , code = code
              , needsSkip = needsSkip
              }

        fun spanOf (kind , vname) =
          case kind of
            "kw" => vname
          | "v" => String.concat [vname , "_span"]
          | _ => raise Fail "spanOf on skip"

        type bind = { kind : string , vname : string , code : string , needsSkip : bool }

        fun emitBindChain indent (binds : bind list) conName valueVars =
          let
            val spanElems = List.filter (fn { kind , ... } => kind = "kw" orelse kind = "v") binds
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

            fun bindPattern kind vname =
              case kind of
                "v" => String.concat [vname , " as { span = " , vname , "_span , ... }"]
              | "vplain" => vname
              | _ => vname

            fun emitBind kind vname code isLast =
              if isLast
              then
                ( print pp (String.concat [indent , "bind " , code , " (fn " , bindPattern kind vname , " =>"])
                ; newline pp
                ; emitReturn ()
                ; print pp (String.implode (List.tabulate (numCloses , fn _ => #")"))
                            ^ ")")
                )
              else
                ( print pp (String.concat [indent , "bind " , code , " (fn " , bindPattern kind vname , " =>"])
                ; newline pp
                )

            fun go (nil : bind list) = ()
              | go [{ vname , code , needsSkip , kind }] =
                  ( if needsSkip
                    then ( print pp (String.concat [indent , "bind skipTrivial (fn _ =>"])
                         ; newline pp
                         )
                    else ()
                  ; emitBind kind vname code true
                  )
              | go ({ vname , code , needsSkip , kind } :: rest) =
                  ( if needsSkip
                    then ( print pp (String.concat [indent , "bind skipTrivial (fn _ =>"])
                         ; newline pp
                         )
                    else ()
                  ; emitBind kind vname code false
                  ; go rest
                  )
          in
            case binds
              of nil => emitReturn ()
               | _ => go binds
          end

        fun emitRuleParser indent defId leftRef rightRef ({ name , spec , ... } : G.rule) =
          let
            val conName = String.concat [ntPascal defId , name]
            val elems = flattenSpec spec

            fun isSelfRef elem =
              case elem of
                G.Nonterminal nid => nid = defId
              | _ => false

            val selfRefIndices =
              List.mapPartial
                (fn (i , elem) => if isSelfRef elem then SOME i else NONE)
                (List.mapi (fn (i , e) => (i , e)) elems)
            val firstSelfRef =
              case selfRefIndices of nil => NONE | i :: _ => SOME i
            val lastSelfRef =
              case selfRefIndices of nil => NONE | _ => SOME (List.last selfRefIndices)

            fun resolveElemAt i elem =
              case elem of
                G.Nonterminal nid =>
                  if nid = defId
                  then
                    let val selfCode =
                      if SOME i = firstSelfRef then leftRef
                      else if SOME i = lastSelfRef then rightRef
                      else leftRef
                    in SOME { code = selfCode , needsSkip = false , isValue = true }
                    end
                  else resolveElem defId leftRef rightRef elem
              | _ => resolveElem defId leftRef rightRef elem

            val resolvedElems =
              List.mapi
                (fn (i , elem) =>
                  case resolveElemAt i elem of
                    NONE => { kind = "skip" , code = "(return ())" , needsSkip = false }
                  | SOME { code , needsSkip , isValue } =>
                      let val kind =
                        case (isValue , elem) of
                          (false , G.Keyword _) => "kw"
                        | (false , _) => "skip"
                        | (true , G.Opt _) => "vplain"
                        | (true , G.Star _) => "vplain"
                        | (true , G.Plus _) => "vplain"
                        | (true , _) => "v"
                      in
                        { kind = kind , code = code , needsSkip = needsSkip }
                      end)
                elems
            val (binds , valueVars , _) =
              List.foldl
                (fn ({ kind , code , needsSkip } , (bs , vs , idx)) =>
                  let
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
                      | "vplain" => vs @ [vname]
                      | _ => vs
                  in
                    (bs @ [{ kind = kind , vname = vname , code = code , needsSkip = needsSkip }] , nextVs , nextIdx)
                  end)
                (nil , nil , 0)
                resolvedElems
          in
            emitBindChain indent binds conName valueVars
          end

        fun emitParser indent { name = id , rules } =
          let
            val inner = indent ^ "  "
            val body = inner ^ "  "
            val selfRef = String.concat ["(deref parse" , ntPascal id , "Dummy)"]

            val nonfixAlts = List.filter (fn a => classifyForm a = FormNonfix) rules
            val fixityAlts = List.filter (fn a => classifyForm a <> FormNonfix) rules

            val precGroups : G.rule list IntMap.dict =
              List.foldl
                (fn (alt as { precedence , ... } : G.rule , m) =>
                  IntMap.insertMerge m precedence [alt] (fn l => l @ [alt]))
                IntMap.empty
                fixityAlts

            val precList = List.rev (IntMap.toList precGroups)
          in
            print pp (String.concat [indent , "(* " , ntName id , " *)"]);
            newline pp;
            print pp (String.concat [indent , "val parse" , ntPascal id , " ="]);
            newline pp;
            print pp (String.concat [inner , "let"]);
            newline pp;

            print pp (String.concat [body , "val parseAtom = fix (fn parseAtom =>"]);
            newline pp;
            print pp (String.concat [body , "let"]);
            newline pp;

            List.appi
              (fn (i , alt as { name , ... } : G.rule) =>
                ( if i > 0 then newline pp else ()
                ; print pp (String.concat [body , "  val parse" , name , " ="])
                ; newline pp
                ; emitRuleParser (body ^ "    ") id selfRef selfRef alt
                ; newline pp
                ))
              nonfixAlts;

            if List.null nonfixAlts then ()
            else newline pp;

            print pp (String.concat [body , "in either"]);
            newline pp;
            List.appi
              (fn (i , { name , ... } : G.rule) =>
                let val prefix = if i = 0 then "[ " else ", "
                in
                  print pp (String.concat [body , prefix , "parse" , name]);
                  newline pp
                end)
              nonfixAlts;
            print pp (String.concat [body , "]"]);
            newline pp;
            print pp (String.concat [body , "end)"]);
            newline pp;
            newline pp;

            List.appi
              (fn (i , (prec , group)) =>
                let
                  val levelName = String.concat ["parseLevel" , Int.toString prec]
                  val levelSelf = levelName
                  val higherName = if i = 0 then "(forget parseAtom)"
                                  else let val (prevPrec , _) = List.nth (precList , i - 1)
                                       in String.concat ["(forget parseLevel" , Int.toString prevPrec , ")"]
                                       end

                  fun refsForAlt alt =
                    case classifyForm alt of
                      FormLeftRecursive => (levelSelf , higherName)
                    | FormRightRecursive => (higherName , levelSelf)
                    | _ => (higherName , higherName)
                in
                  print pp (String.concat [body , "val " , levelName , " = fix (fn " , levelSelf , " =>"]);
                  newline pp;

                  print pp (String.concat [body , "let"]);
                  newline pp;

                  List.app
                    (fn alt as { name , ... } : G.rule =>
                      let val (leftRef , rightRef) = refsForAlt alt
                      in
                        print pp (String.concat [body , "  val parse" , name , " ="]);
                        newline pp;
                        emitRuleParser (body ^ "    ") id leftRef rightRef alt;
                        newline pp;
                        newline pp
                      end)
                    group;

                  print pp (String.concat [body , "in either"]);
                  newline pp;
                  print pp (String.concat [body , "[ " , higherName]);
                  newline pp;
                  List.app
                    (fn { name , ... } : G.rule =>
                      ( print pp (String.concat [body , ", parse" , name])
                      ; newline pp
                      ))
                    group;
                  print pp (String.concat [body , "]"]);
                  newline pp;
                  print pp (String.concat [body , "end)"]);
                  newline pp;
                  newline pp
                end)
              precList;

            print pp (String.concat [inner , "in"]);
            newline pp;
            ( case precList
                of nil => print pp (String.concat [body , "forget parseAtom"])
                 | _ =>
                     let val (lastPrec , _) = List.last precList
                     in print pp (String.concat [body , "forget parseLevel" , Int.toString lastPrec])
                     end
            );
            newline pp;
            print pp (String.concat [inner , "end"]);
            newline pp;
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

        fun countValueElems r =
          case r of
            G.Terminal _ => 1
          | G.Nonterminal _ => 1
          | G.Star r => countValueElems r
          | G.Plus r => countValueElems r
          | G.Opt r => countValueElems r
          | G.Seq rs => List.foldl (fn (r , n) => n + countValueElems r) 0 rs
          | G.Keyword _ => 0

        fun emitPrintRepeat indent buf r idx =
          let
            val vname = String.concat ["v" , Int.toString idx]
            val innerElems = flattenSpec r
            val numInner = countValueElems r
            val innerPat =
              case numInner of
                0 => "_"
              | 1 => vname
              | n => String.concat ["(" , String.concatWith " , "
                  (List.tabulate (n , fn i => String.concat ["v" , Int.toString (idx + i)])) , ")"]
          in
            if isValueElem r
            then
              ( print pp (String.concat [indent , "List.app (fn " , innerPat , " =>"])
              ; newline pp
              ; if List.length innerElems > 1
                then ( print pp (String.concat [indent , "  ("])
                     ; newline pp
                     ; ignore (emitPrintElems (indent ^ "  ") buf innerElems idx)
                     ; print pp ")"
                     )
                else ignore (emitPrintElems (indent ^ "  ") buf innerElems idx)
              ; print pp (String.concat [") " , vname])
              ; SOME (idx + 1)
              )
            else
              ( ignore (emitPrintElems indent buf innerElems idx)
              ; NONE
              )
          end

        and emitPrintElem indent buf elem idx =
          case elem of
            G.Keyword kid =>
              ( print pp (String.concat
                  [indent , "PrintBuffer.push " , buf , " \"" , String.toString (kwName kid) , "\" 0"])
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
          | G.Opt r =>
              let
                val vname = String.concat ["v" , Int.toString idx]
                val innerElems = flattenSpec r
                val numInner = countValueElems r
                val innerPat =
                  case numInner of
                    0 => "_"
                  | 1 => vname
                  | n => String.concat ["(" , String.concatWith " , "
                      (List.tabulate (n , fn i => String.concat ["v" , Int.toString (idx + i)])) , ")"]
              in
                if isValueElem r
                then
                  ( print pp (String.concat [indent , "(case " , vname , " of NONE => ()"])
                  ; newline pp
                  ; print pp (String.concat [indent , "| SOME " , innerPat , " =>"])
                  ; newline pp
                  ; if List.length innerElems > 1
                    then ( print pp (String.concat [indent , "  ("])
                         ; newline pp
                         ; ignore (emitPrintElems (indent ^ "  ") buf innerElems idx)
                         ; print pp ")"
                         )
                    else ignore (emitPrintElems (indent ^ "  ") buf innerElems idx)
                  ; print pp ")"
                  ; SOME (idx + 1)
                  )
                else
                  ( ignore (emitPrintElems indent buf innerElems idx)
                  ; NONE
                  )
              end
          | G.Star r => emitPrintRepeat indent buf r idx
          | G.Plus r => emitPrintRepeat indent buf r idx
          | _ => NONE

        and emitPrintElems indent buf elems startIdx =
          let
            val (idx , _) =
              List.foldl
                (fn (elem , (idx , first)) =>
                  let
                    val () = if first then () else (print pp ";"; newline pp)
                    val result = emitPrintElem indent buf elem idx
                    val nextIdx =
                      case result of
                        SOME idx => idx
                      | NONE => idx
                  in
                    (nextIdx , false)
                  end)
                (startIdx , true)
                elems
          in
            idx
          end

        fun emitPrintAlt indent defName ({ spec , name , ... } : G.rule) =
          let
            val elems = flattenSpec spec
            val conName = toPascalCase defName ^ name
            val valueElems = List.filter isValueElem elems
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
            in
              case elems of
                nil => print pp (String.concat [body , "()"])
              | _ =>
                let val needsParens = List.length elems > 1
                in
                  if needsParens then (print pp (String.concat [body , "("]); newline pp) else ();
                  emitPrintElems body "buf" elems 0;
                  if needsParens then (newline pp; print pp (String.concat [body , ")"])) else ();
                  ()
                end
            end
          end

        fun emitPrinter indent isFirst { name = id , rules } =
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
              rules;
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
            print pp "  val keywords =";
            break pp 0;
            ( case keywordList of
                nil => print pp "    []"
              | ((firstId , firstStr) :: rest) =>
                  ( print pp (String.concat
                      ["    [ (\"" , String.toString firstStr , "\" , " , Int.toString firstId , ")"])
                  ; List.app
                      (fn (id , s) =>
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
            print pp "  val skipTrivial = optionalLongest (remove (fn";
            break pp 0;
            print pp "    LexInternal.TokenTrivial _ => true";
            break pp 0;
            print pp "  | _ => false))";
            break pp 0;
            print pp "  fun parseTerminal proj = terminal (fn";
            break pp 0;
            print pp "    LexInternal.TokenOther (v , sp) => (case proj v of SOME t => SOME { node = t , span = sp } | NONE => NONE)";
            break pp 0;
            print pp "  | _ => NONE)";
            break pp 0;
            List.app
              (fn name =>
                let
                  val terminalName = toPascalCase name
                  val proj =
                    if List.length terminalNames = 1
                    then String.concat ["(fn Terminal" , terminalName , " v => SOME v)"]
                    else String.concat ["(fn Terminal" , terminalName , " v => SOME v | _ => NONE)"]
                in
                  print pp (String.concat
                    ["  val parseTerminal" , terminalName , " = parseTerminal " , proj]);
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
            ( case terminalNames of
                nil =>
                  print pp "fun lex s pos = LexInternal.lex s pos keywords Trivial.lex []"
              | _ =>
                  ( print pp "fun lex s pos = LexInternal.lex s pos keywords Trivial.lex"
                  ; break pp 0
                  ; List.appi
                      (fn (i , name) =>
                        let
                          val terminalName = toPascalCase name
                          val prefix = if i = 0 then "  [ " else "  , "
                        in
                          print pp (String.concat
                            [prefix , "fn x => case Terminals." , terminalName , ".lex x of SOME (v , s , p) => SOME (Terminal" , terminalName , " v , s , p) | NONE => NONE"]);
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
