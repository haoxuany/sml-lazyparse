structure Codegen :
  sig
    val codegen : string -> IL.t -> unit
  end =
  struct

    structure I = IL
    structure IM = I.IdMap

    fun toSnakeCase s =
      let
        fun go ( nil , _ ) = nil
          | go ( c :: cs , isFirst ) =
              if Char.isUpper c
              then if isFirst
                   then Char.toLower c :: go ( cs , false )
                   else #"_" :: Char.toLower c :: go ( cs , false )
              else c :: go ( cs , false )
      in
        String.implode (go ( String.explode s , true ))
      end

    fun toPascalCase s =
      let
        fun go ( nil , _ ) = nil
          | go ( #"_" :: cs , _ ) = go ( cs , true )
          | go ( c :: cs , true ) = Char.toUpper c :: go ( cs , false )
          | go ( c :: cs , false ) = c :: go ( cs , false )
      in
        String.implode (go ( String.explode s , true ))
      end

    fun nameOfId ( m : string IM.dict ) id =
      case IM.find m id of
        SOME name => name
      | NONE => raise Fail "unknown id"

    fun codegen ( name : string ) ( { nonterminals , terminals , keywords , datatypes , definitions } : I.t ) =
      let
        val filename = name ^ ".sml"
        val functorName = toPascalCase name
        val out = TextIO.openOut filename
        val pp = PrettyPrint.makeStream out 80
        open PrettyPrint

        val terminalNames = List.map (fn ( _ , name ) => name) (IM.toList terminals)

        val keywordList = IM.toList keywords

        fun ntName id = nameOfId nonterminals id
        fun ntSnake id = toSnakeCase (ntName id)
        fun ntPascal id = toPascalCase (ntName id)
        fun tName id = nameOfId terminals id
        fun tPascal id = toPascalCase (tName id)
        fun kwName id = nameOfId keywords id

        fun varName v = String.concat ["v" , Int.toString v]

        val terminalWhere = "TERMINAL where type 'a stream = 'a Stream.stream"
        val terminalPrintableWhere = "TERMINAL_PRINTABLE where type 'a stream = 'a Stream.stream"

        fun tyToString ty =
          case ty of
            I.TyTerminal id =>
              String.concat ["Terminals." , tPascal id , ".t annot"]
          | I.TyNonterminal id => ntSnake id
          | I.TyList t => tyToString t ^ " list"
          | I.TyOption t => tyToString t ^ " option"
          | I.TyTuple ts =>
              String.concat ["(" , String.concatWith " * " (List.map tyToString ts) , ")"]

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
                ( break pp 0
                ; print pp (String.concat ["structure " , toPascalCase name , " : " , terminalPrintableWhere])
                ))
              terminalNames
          ; closeBox pp
          ; break pp 0
          ; print pp "end"
          ; closeBox pp
          ; break pp 0
          ; print pp ") :>"
          ; break pp 0
          )

        fun emitConstructor defName ( { name , ty } : { name : string , ty : I.ty list } ) =
          let
            val conName = toPascalCase defName ^ name
            val types = List.map tyToString ty
          in
            case types of
              nil => conName
            | _ => String.concat [conName , " of " , String.concatWith " * " types]
          end

        fun emitDatatype isFirst ( { id , rules } : { id : I.id , rules : { name : string , ty : I.ty list } list } ) =
          let
            val defName = ntName id
            val keyword = if isFirst then "datatype" else "and"
            val cons = List.map (emitConstructor defName) rules
          in
            case cons of
              nil => ()
            | first :: rest =>
                ( openBox pp Vertical 2
                ; print pp (String.concat [keyword , " " , ntSnake id , "' = " , first])
                ; List.app (fn c => ( break pp 0 ; print pp (String.concat ["| " , c]) )) rest
                ; closeBox pp
                ; break pp 0
                )
          end

        fun emitWithtype isFirst id =
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
              (fn ( i , { id , rules } ) =>
                let
                  val keyword = if i = 0 then "datatype" else "and"
                  val cons = List.map (emitConstructor (ntName id)) rules
                in
                  case cons of
                    nil => ()
                  | first :: rest =>
                      ( print pp (String.concat [keyword , " " , ntSnake id , "' = " , first])
                      ; List.app (fn c => ( break pp 0 ; print pp (String.concat ["| " , c]) )) rest
                      ; break pp 0
                      )
                end)
              datatypes
          ; List.appi
              (fn ( i , { id , ... } : { id : I.id , rules : { name : string , ty : I.ty list } list } ) =>
                let val keyword = if i = 0 then "withtype" else "and"
                in
                  print pp (String.concat [keyword , " " , ntSnake id , " = " , ntSnake id , "' annot"]);
                  break pp 0
                end)
              datatypes
          ; break pp 0
          ; print pp "type 'a parser"
          ; break pp 0
          ; print pp "type token_stream"
          ; break pp 0
          ; print pp "val lex : char Stream.stream -> Annot.pos -> token_stream"
          ; break pp 0
          ; List.app
              (fn { name , ... } : I.definition =>
                ( print pp (String.concat ["val parse" , ntPascal name , " : " , ntSnake name , " parser"])
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
            (fn { name , ... } : I.definition =>
              ( print pp (String.concat
                  ["  val parse" , ntPascal name , "Dummy : " , ntSnake name , " t_dummy = dummy ()"])
              ; break pp 0
              ))
            definitions

        fun parserCode selfRef higherRef parser =
          case parser of
            I.Terminal tid =>
              String.concat ["(parseTerminal" , tPascal tid , ")"]
          | I.Keyword kid =>
              String.concat ["(keyword " , Int.toString kid , ")"]
          | I.Ref I.Self =>
              String.concat ["(parseNonterminal " , selfRef , ")"]
          | I.Ref I.Higher =>
              String.concat ["(parseNonterminal " , higherRef , ")"]
          | I.Ref (I.Other nid) =>
              String.concat ["(parseNonterminal (deref parse" , ntPascal nid , "Dummy))"]
          | _ => raise Fail "parserCode: sub-parser should be handled by emitBind"

        fun emitBind indent selfRef higherRef var parser emitRest =
          let
            fun emitSubCmd wrapName subCmd =
              ( print pp (String.concat [indent , "bind (" , wrapName , " ("]);
                newline pp;
                emitCmdBody (indent ^ "  ") selfRef higherRef subCmd;
                print pp "))";
                newline pp;
                print pp (String.concat [indent , "(fn " , varName var , " =>"]);
                newline pp;
                emitRest ();
                print pp ")"
              )
          in
            case parser of
              I.Star subCmd => emitSubCmd "starLongest" subCmd
            | I.Plus subCmd => emitSubCmd "plusLongest" subCmd
            | I.Opt subCmd => emitSubCmd "optionalLongest" subCmd
            | I.Seq subCmd =>
                ( print pp (String.concat [indent , "bind ("]);
                  newline pp;
                  emitCmdBody (indent ^ "  ") selfRef higherRef subCmd;
                  print pp ")";
                  newline pp;
                  print pp (String.concat [indent , "(fn " , varName var , " =>"]);
                  newline pp;
                  emitRest ();
                  print pp ")"
                )
            | _ =>
                let val code = parserCode selfRef higherRef parser
                in
                  print pp (String.concat [indent , "bind " , code , " (fn " , varName var , " =>"]);
                  newline pp;
                  emitRest ();
                  print pp ")"
                end
          end

        and emitCmdBody indent selfRef higherRef cmd =
          case cmd of
            I.Return { args , allVars } =>
              let
                fun nodeOf v = String.concat ["(#node " , varName v , ")"]
                val nodeExpr =
                  case args of
                    nil => "()"
                  | [v] => nodeOf v
                  | _ => String.concat ["(" , String.concatWith " , "
                      (List.map nodeOf args) , ")"]
                val spanArgs = String.concat ["[ " , String.concatWith " , "
                    (List.map (fn v => String.concat ["annot_add " , varName v]) allVars) , " ]"]
              in
                print pp (String.concat [indent , "return_node " , nodeExpr , " " , spanArgs])
              end
          | I.Bind { var , parser , andthen } =>
              emitBind indent selfRef higherRef var parser
                (fn () => emitCmdBody indent selfRef higherRef andthen)

        fun emitRuleParser indent defId selfRef higherRef ( { name , cmd } : I.rule ) =
          let
            val conName = String.concat [ntPascal defId , name]
            fun findArgs cmd =
              case cmd of
                I.Return { args , allVars = _ } => args
              | I.Bind { andthen , ... } => findArgs andthen
            val args = findArgs cmd
            val conExpr =
              case args of
                nil => String.concat ["(fn () => " , conName , ")"]
              | [_] => conName
              | _ => conName
          in
            print pp (String.concat [indent , "create " , conExpr , " ("]);
            newline pp;
            emitCmdBody (indent ^ "  ") selfRef higherRef cmd;
            print pp ")"
          end

        fun emitParser indent ( { name = defId , atoms , levels } : I.definition ) =
          let
            val inner = indent ^ "  "
            val body = inner ^ "  "
            val selfRef = String.concat ["(deref parse" , ntPascal defId , "Dummy)"]

            val precList = levels
          in
            print pp (String.concat [indent , "(* " , ntName defId , " *)"]);
            newline pp;
            print pp (String.concat [indent , "val parse" , ntPascal defId , " ="]);
            newline pp;
            print pp (String.concat [inner , "let"]);
            newline pp;

            print pp (String.concat [body , "val parseAtom = fix (fn parseAtom =>"]);
            newline pp;
            print pp (String.concat [body , "let"]);
            newline pp;

            List.appi
              (fn ( i , rule as { name , ... } : I.rule ) =>
                ( if i > 0 then newline pp else ()
                ; print pp (String.concat [body , "  val parse" , name , " ="])
                ; newline pp
                ; emitRuleParser (body ^ "    ") defId selfRef selfRef rule
                ; newline pp
                ))
              atoms;

            if List.null atoms then ()
            else newline pp;

            print pp (String.concat [body , "in either"]);
            newline pp;
            List.appi
              (fn ( i , { name , ... } : I.rule ) =>
                let val prefix = if i = 0 then "[ " else ", "
                in
                  print pp (String.concat [body , prefix , "parse" , name]);
                  newline pp
                end)
              atoms;
            print pp (String.concat [body , "]"]);
            newline pp;
            print pp (String.concat [body , "end)"]);
            newline pp;
            newline pp;

            List.appi
              (fn ( i , { precedence , rules } : I.level ) =>
                let
                  val levelName = String.concat ["parseLevel" , Int.toString precedence]
                  val higherName = if i = 0 then "(forget parseAtom)"
                                   else let val { precedence = prevPrec , ... } : I.level = List.nth ( precList , i - 1 )
                                        in String.concat ["(forget parseLevel" , Int.toString prevPrec , ")"]
                                        end
                in
                  print pp (String.concat [body , "val " , levelName , " = fix (fn " , levelName , " =>"]);
                  newline pp;

                  print pp (String.concat [body , "let"]);
                  newline pp;

                  List.app
                    (fn rule as { name , ... } : I.rule =>
                      ( print pp (String.concat [body , "  val parse" , name , " ="])
                      ; newline pp
                      ; emitRuleParser (body ^ "    ") defId levelName higherName rule
                      ; newline pp
                      ; newline pp
                      ))
                    rules;

                  print pp (String.concat [body , "in either"]);
                  newline pp;
                  print pp (String.concat [body , "[ " , higherName]);
                  newline pp;
                  List.app
                    (fn { name , ... } : I.rule =>
                      ( print pp (String.concat [body , ", parse" , name])
                      ; newline pp
                      ))
                    rules;
                  print pp (String.concat [body , "]"]);
                  newline pp;
                  print pp (String.concat [body , "end)"]);
                  newline pp;
                  newline pp
                end)
              precList;

            print pp (String.concat [inner , "in"]);
            newline pp;
            ( case precList of
                nil => print pp (String.concat [body , "forget parseAtom"])
              | _ =>
                  let val { precedence = lastPrec , ... } : I.level = List.last precList
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
            (fn { name , ... } : I.definition =>
              ( print pp (String.concat
                  ["val () = set parse" , ntPascal name , "Dummy parse" , ntPascal name])
              ; break pp 0
              ))
            definitions

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
              (fn ( i , dt ) => emitDatatype (i = 0) dt)
              datatypes;
            List.appi
              (fn ( i , { id , ... } : { id : I.id , rules : { name : string , ty : I.ty list } list } ) =>
                emitWithtype (i = 0) id)
              datatypes;
            break pp 0;
            ( case terminalNames of
                nil => ()
              | first :: rest =>
                  ( print pp (String.concat
                      ["datatype terminal_token = Terminal" , toPascalCase first , " of Terminals." , toPascalCase first , ".t"])
                  ; List.app
                      (fn name =>
                        ( break pp 0
                        ; print pp (String.concat
                            ["| Terminal" , toPascalCase name , " of Terminals." , toPascalCase name , ".t"])
                        ))
                      rest
                  ; break pp 0
                  )
            );
            break pp 0;
            openBox pp Vertical 2;
            print pp (String.concat ["structure Internal = ParseInternal ("]);
            break pp 0;
            print pp "val table_size = table_size";
            break pp 0;
            print pp "structure Stream = Stream";
            break pp 0;
            print pp "structure Trivial = Trivial";
            break pp 0;
            print pp "type terminal = terminal_token";
            break pp 0;
            print pp "val keywords =";
            break pp 0;
            ( case keywordList of
                nil => print pp "  []"
              | ( ( firstId , firstStr ) :: rest ) =>
                  ( print pp (String.concat
                      ["  [ (\"" , String.toString firstStr , "\" , " , Int.toString firstId , ")"])
                  ; List.app
                      (fn ( id , s ) =>
                        ( break pp 0
                        ; print pp (String.concat
                            ["  , (\"" , String.toString s , "\" , " , Int.toString id , ")"])
                        ))
                      rest
                  ; break pp 0
                  ; print pp "  ]"
                  )
            );
            closeBox pp;
            break pp 0;
            print pp ")";
            break pp 0;
            print pp "open Internal";
            break pp 0;
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
                    ["val parseTerminal" , terminalName , " = parseTerminal " , proj]);
                  break pp 0
                end)
              terminalNames;
            emitBackpatchRefs ();
            break pp 0;
            ( case terminalNames of
                nil =>
                  print pp "val lex = lex []"
              | _ =>
                  ( print pp "val lex = lex"
                  ; break pp 0
                  ; List.appi
                      (fn ( i , name ) =>
                        let
                          val terminalName = toPascalCase name
                          val prefix = if i = 0 then "  [ " else "  , "
                        in
                          print pp (String.concat
                            [prefix , "addLexer Terminals." , terminalName , ".lex Terminal" , terminalName]);
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
            print pp "val parse = parser";
          closeBox pp;
          break pp 0;
          break pp 0;
          print pp "end";
        closeBox pp;
        flush pp;
        TextIO.closeOut out
      end

  end
