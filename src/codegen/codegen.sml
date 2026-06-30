structure Codegen :
  sig
    val codegen : { dir : string , name : string } -> IL.t -> unit
  end =
  struct

    structure I = IL
    structure IM = I.IdMap

    fun toSnakeCase s =
      let
        fun toSnakeCase x =
          case x of
            ( nil , _ ) => nil
          | ( c :: cs , isFirst ) =>
              if Char.isUpper c
              then if isFirst
                   then Char.toLower c :: toSnakeCase ( cs , false )
                   else #"_" :: Char.toLower c :: toSnakeCase ( cs , false )
              else c :: toSnakeCase ( cs , false )
      in
        String.implode (toSnakeCase ( String.explode s , true ))
      end

    fun toModuleCase s =
      let
        fun toModuleCase x =
          case x of
            ( nil , _ ) => nil
          | ( #"_" :: cs , _ ) => toModuleCase ( cs , true )
          | ( c :: cs , true ) => Char.toUpper c :: toModuleCase ( cs , false )
          | ( c :: cs , false ) => c :: toModuleCase ( cs , false )
      in
        String.implode (toModuleCase ( String.explode s , true ))
      end

    fun toUpperCase s = String.map Char.toUpper s

    fun nameOfId ( m : string IM.dict ) id =
      case IM.find m id of
        SOME name => name
      | NONE => raise Fail "Impossible"

    fun codegen ( { dir , name } : { dir : string , name : string } )
      ( { nonterminals , terminals , keywords , datatypes , definitions } : I.t ) =
      let
        val sigName = String.concat [toUpperCase (toSnakeCase name) , "_AST"]
        val out = TextIO.openOut (OS.Path.joinDirFile { dir = dir , file = name ^ ".sml" })

        val pp = PrettyPrint.makeStream out 80
        open PrettyPrint
        val print = fn l => print pp (String.concat l)
        fun break () = PrettyPrint.break pp 0
        val newline = fn () => PrettyPrint.newline pp

        val terminalNames =
          List.map (fn ( _ , name ) => name) (IM.toList terminals)

        fun ntName id = nameOfId nonterminals id
        fun ntSnake id = toSnakeCase (ntName id)
        fun ntModule id = toModuleCase (ntName id)
        fun tModule id = toModuleCase (nameOfId terminals id)
        fun tSnake id = toSnakeCase (nameOfId terminals id)

        fun varName v = String.concat ["v" , Int.toString v]

        val terminalWhere = "TERMINAL"

        fun tyToString ty =
          case ty of
            I.TyTerminal id => tSnake id ^ " annot"
          | I.TyNonterminal id => ntSnake id
          | I.TyList t => tyToString t ^ " list"
          | I.TyOption t => tyToString t ^ " option"
          | I.TyTuple ts =>
              String.concat ["(" , String.concatWith " * " (List.map tyToString ts) , ")"]

        fun constructorStr defName ( { name , ty } : { name : string , ty : I.ty list } ) =
          let
            val conName = toModuleCase defName ^ name
            val types = List.map tyToString ty
          in
            case types of
              nil => conName
            | _ => String.concat [conName , " of " , String.concatWith " * " types]
          end

        fun emitDatatypes () =
          ( List.appi
              (fn ( i , { id , rules } ) =>
                let
                  val defName = ntName id
                  val keyword = if i = 0 then "datatype" else "and"
                  val cons = List.map (constructorStr defName) rules
                in
                  case cons of
                    nil => ()
                  | first :: rest =>
                      ( openBox pp Vertical 2
                      ; print [keyword , " " , ntSnake id , "' = " , first]
                      ; List.app (fn c => ( break () ; print ["| " , c] )) rest
                      ; closeBox pp
                      ; break ()
                      )
                end)
              datatypes
          ; List.appi
              (fn ( i , { id , ... } : { id : I.id , rules : { name : string , ty : I.ty list } list } ) =>
                let val keyword = if i = 0 then "withtype" else "and"
                in
                  print [keyword , " " , ntSnake id , " = " , ntSnake id , "' annot"]
                  ; break ()
                end)
              datatypes
          )

        fun parserCode selfRef higherRef parser =
          case parser of
            I.Terminal tid =>
              String.concat ["(parseTerminal" , tModule tid , ")"]
          | I.Keyword kid =>
              String.concat ["(keyword " , Int.toString kid , ")"]
          | I.Ref I.Self =>
              String.concat ["(parseNonterminal " , selfRef , ")"]
          | I.Ref I.Higher =>
              String.concat ["(parseNonterminal " , higherRef , ")"]
          | I.Ref (I.Other nid) =>
              String.concat ["(parseNonterminal (deref parse" , ntModule nid , "Dummy))"]
          | _ => raise Fail "parserCode: sub-parser should be handled by emitBind"

        fun emitBind indent selfRef higherRef var parser emitRest =
          let
            fun emitSubCmd wrapName subCmd =
              ( print [indent , "bind (" , wrapName , " ("]
              ; newline ()
              ; emitCmdBody (indent ^ "  ") selfRef higherRef subCmd
              ; print ["))"]
              ; newline ()
              ; print [indent , "(fn " , varName var , " =>"]
              ; newline ()
              ; emitRest ()
              ; print [")"]
              )
          in
            case parser of
              I.Star subCmd => emitSubCmd "starLongest" subCmd
            | I.Plus subCmd => emitSubCmd "plusLongest" subCmd
            | I.Opt subCmd => emitSubCmd "optionalLongest" subCmd
            | I.Seq subCmd =>
                ( print [indent , "bind ("]
                ; newline ()
                ; emitCmdBody (indent ^ "  ") selfRef higherRef subCmd
                ; print [")"]
                ; newline ()
                ; print [indent , "(fn " , varName var , " =>"]
                ; newline ()
                ; emitRest ()
                ; print [")"]
                )
            | _ =>
                let val code = parserCode selfRef higherRef parser
                in
                  print [indent , "bind " , code , " (fn " , varName var , " =>"]
                  ; newline ()
                  ; emitRest ()
                  ; print [")"]
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
              in
                case allVars of
                  nil =>
                    print [indent , "empty " , nodeExpr]
                | _ =>
                    let
                      val spanArgs = String.concat ["[ " , String.concatWith " , "
                          (List.map (fn v => String.concat ["annot_add " , varName v]) allVars) , " ]"]
                    in
                      print [indent , "return_node " , nodeExpr , " " , spanArgs]
                    end
              end
          | I.Bind { var , parser , andthen } =>
              emitBind indent selfRef higherRef var parser
                (fn () => emitCmdBody indent selfRef higherRef andthen)

        fun emitRuleParser indent defId selfRef higherRef ( { name , cmd } : I.rule ) =
          let
            fun findArgs cmd =
              case cmd of
                I.Return { args , allVars = _ } => args
              | I.Bind { andthen , ... } => findArgs andthen
            val conName = String.concat [ntModule defId , name]
            val conExpr =
              case findArgs cmd of
                nil => String.concat ["(fn () => " , conName , ")"]
              | _ => conName
          in
            print [indent , "create " , conExpr , " \"" , ntName defId , "\" \"" , name , "\" ("]
            ; newline ()
            ; emitCmdBody (indent ^ "  ") selfRef higherRef cmd
            ; print [")"]
          end

        fun emitParser indent ( { name = defId , atoms , levels } : I.definition ) =
          let
            val inner = indent ^ "  "
            val body = inner ^ "  "
            val selfRef = String.concat ["(deref parse" , ntModule defId , "Dummy)"]
          in
            print [indent , "(* " , ntName defId , " *)"]
            ; newline ()
            ; print [indent , "val parse" , ntModule defId , " ="]
            ; newline ()
            ; print [inner , "let"]
            ; newline ()

            ; print [body , "val parseAtom = fix (fn parseAtom =>"]
            ; newline ()
            ; print [body , "let"]
            ; newline ()

            ; List.appi
                (fn ( i , rule as { name , ... } : I.rule ) =>
                  ( if i > 0 then newline () else ()
                  ; print [body , "  val parse" , name , " ="]
                  ; newline ()
                  ; emitRuleParser (body ^ "    ") defId selfRef selfRef rule
                  ; newline ()
                  ))
                atoms

            ; if List.null atoms then ()
              else newline ()

            ; print [body , "in either"]
            ; newline ()
            ; List.appi
                (fn ( i , { name , ... } : I.rule ) =>
                  let val prefix = if i = 0 then "[ " else ", "
                  in
                    print [body , prefix , "parse" , name]
                    ; newline ()
                  end)
                atoms
            ; print [body , "]"]
            ; newline ()
            ; print [body , "end)"]
            ; newline ()
            ; newline ()

            ; List.appi
                (fn ( i , { precedence , rules } : I.level ) =>
                  let
                    val levelName = String.concat ["parseLevel" , Int.toString precedence]
                    val higherName =
                      if i = 0 then "(forget parseAtom)"
                      else let val { precedence , ... } : I.level = List.nth ( levels , i - 1 )
                           in String.concat ["(forget parseLevel" , Int.toString precedence , ")"]
                           end
                  in
                    print [body , "val " , levelName , " = fix (fn " , levelName , " =>"]
                    ; newline ()

                    ; print [body , "let"]
                    ; newline ()

                    ; List.app
                        (fn rule as { name , ... } : I.rule =>
                          ( print [body , "  val parse" , name , " ="]
                          ; newline ()
                          ; emitRuleParser (body ^ "    ") defId levelName higherName rule
                          ; newline ()
                          ; newline ()
                          ))
                        rules

                    ; print [body , "in either"]
                    ; newline ()
                    ; print [body , "[ " , higherName]
                    ; newline ()
                    ; List.app
                        (fn { name , ... } : I.rule =>
                          ( print [body , ", parse" , name]
                          ; newline ()
                          ))
                        rules
                    ; print [body , "]"]
                    ; newline ()
                    ; print [body , "end)"]
                    ; newline ()
                    ; newline ()
                  end)
                levels

            ; print [inner , "in"]
            ; newline ()
            ; ( case levels of
                  nil => print [body , "longest (forget parseAtom)"]
                | _ =>
                    let val { precedence , ... } : I.level = List.last levels
                    in print [body , "longest (forget parseLevel" , Int.toString precedence , ")"]
                    end
              )
            ; newline ()
            ; print [inner , "end"]
            ; newline ()
            ; newline ()
          end

      in
        openBox pp Vertical 0
        ; newline ()

        (* AST signature *)
        ; openBox pp Vertical 2
        ; print ["signature " , sigName , " = sig"]
        ; break ()
        ; print ["type 'a annot = { node : 'a , span : Annot.span }"]
        ; break ()
        ; break ()
        ; print ["(* terminals *)"]
        ; break ()
        ; List.app
            (fn name =>
              ( print ["type " , toSnakeCase name]
              ; break ()
              ))
            terminalNames
        ; break ()
        ; print ["(* nonterminals *)"]
        ; break ()
        ; emitDatatypes ()
        ; closeBox pp
        ; break ()
        ; print ["end"]
        ; break ()

        (* functor header *)
        ; break ()
        ; openBox pp Vertical 2
        ; print ["functor " , toModuleCase name , "Parser ("]
        ; break ()
        ; print ["structure Trivial : " , terminalWhere]
        ; break ()
        ; openBox pp Vertical 2
        ; print ["structure Terminals : sig"]
        ; List.app
            (fn name =>
              ( break ()
              ; print ["structure " , toModuleCase name , " : " , terminalWhere]
              ))
            terminalNames
        ; closeBox pp
        ; break ()
        ; print ["end"]
        ; closeBox pp
        ; break ()
        ; print [") :>"]
        ; break ()

        (* seal signature *)
        ; openBox pp Vertical 2
        ; print ["sig"]
        ; break ()
        ; print ["include " , sigName]
        ; break ()
        ; List.app
            (fn name =>
              ( print ["where type " , toSnakeCase name , " = Terminals." , toModuleCase name , ".t"]
              ; break ()
              ))
            terminalNames
        ; break ()
        ; print ["type 'a parser"]
        ; break ()
        ; break ()
        ; ( case terminalNames of
              nil => ()
            | first :: rest =>
                ( print
                  [ "datatype terminal_token = Terminal"
                  , toModuleCase first , " of "
                  , toSnakeCase first]
                ; List.app
                    (fn name =>
                      ( break ()
                      ; print
                        [ "| Terminal"
                        , toModuleCase name
                        , " of "
                        , toSnakeCase name]
                      ))
                    rest
                ; break ()
                ; break ()
                )
          )
        ; openBox pp Vertical 2
        ; print ["structure TokenStream : sig"]
        ; break ()
        ; print ["type t"]
        ; break ()
        ; print ["datatype token ="]
        ; break ()
        ; print ["  Keyword of String.string * Annot.span"]
        ; break ()
        ; print ["| Terminal of terminal_token * Annot.span"]
        ; break ()
        ; break ()
        ; print ["datatype front = Nil | Cons of token * t"]
        ; break ()
        ; print ["val front : t -> front"]
        ; break ()
        ; break ()
        ; print ["val pos : t -> Annot.pos"]
        ; closeBox pp
        ; break ()
        ; print ["end"]
        ; break ()
        ; break ()
        ; print ["exception LexError of Char.char * Annot.pos"]
        ; break ()
        ; print ["val lex : Char.char Stream.stream -> Annot.pos -> TokenStream.t"]
        ; break ()
        ; break ()
        ; List.app
            (fn { name , ... } : I.definition =>
              ( print ["val parse" , ntModule name , " : " , ntSnake name , " parser"]
              ; break ()
              ))
            definitions
        ; break ()
        ; print ["datatype 'a result ="]
        ; break ()
        ; print ["  Success of ('a * TokenStream.t) list"]
        ; break ()
        ; print ["| Fail of ParseError.t"]
        ; break ()
        ; print ["val parse : 'a parser -> TokenStream.t -> 'a result"]
        ; break ()
        ; closeBox pp
        ; break ()
        ; print ["end ="]

        (* struct body *)
        ; break ()
        ; print ["struct"]
        ; break ()
        ; openBox pp Vertical 2
          ; break ()
          ; print ["type 'a annot = { node : 'a , span : Annot.span }"]
          ; break ()
          ; List.app
              (fn name =>
                ( print ["type " , toSnakeCase name , " = Terminals." , toModuleCase name , ".t"]
                ; break ()
                ))
              terminalNames
          ; break ()
          ; emitDatatypes ()
          ; break ()
          ; ( case terminalNames of
                nil => ()
              | first :: rest =>
                  ( print 
                    [ "datatype terminal_token = Terminal" 
                    , toModuleCase first , " of Terminals." 
                    , toModuleCase first , ".t"]
                  ; List.app
                      (fn name =>
                        ( break ()
                        ; print 
                          [ "| Terminal" 
                          , toModuleCase name 
                          , " of Terminals." 
                          , toModuleCase name , ".t"]
                        ))
                      rest
                  ; break ()
                  )
            )
          ; break ()
          ; openBox pp Vertical 2
          ; print ["structure Internal = ParseInternal ("]
          ; break ()
          ; print ["structure Trivial = Trivial"]
          ; break ()
          ; openBox pp Vertical 2
          ; print ["structure Terminal = struct"]
          ; break ()
          ; print ["type t = terminal_token"]
          ; break ()
          ; ( case terminalNames of
                nil =>
                  ( print ["fun name _ = \"\""]
                  ; break ()
                  )
              | _ =>
                  ( print ["val name = fn v => (case v of"]
                  ; break ()
                  ; List.appi
                      (fn ( i , name ) =>
                        let val prefix = if i = 0 then "  " else "| "
                        in
                          print [prefix , "Terminal" , toModuleCase name , " _ => \"" , name , "\""]
                          ; break ()
                        end)
                      terminalNames
                  ; print [")"]
                  ; break ()
                  )
            )
          ; print ["val lex ="]
          ; break ()
          ; ( case terminalNames of
                nil => print ["  []"]
              | _ =>
                  ( List.appi
                      (fn ( i , name ) =>
                        let
                          val name = toModuleCase name
                          val prefix = if i = 0 then "  [ " else "  , "
                        in
                          print [prefix , "(fn ts =>"]
                          ; break ()
                          ; print ["      case Terminals." , name , ".lex ts of"]
                          ; break ()
                          ; print ["        SOME (v , ts') => SOME (Terminal" , name , " v , ts')"]
                          ; break ()
                          ; print ["      | NONE => NONE)"]
                          ; break ()
                        end)
                      terminalNames
                  ; print ["  ]"]
                  )
            )
          ; closeBox pp
          ; break ()
          ; print ["end"]
          ; break ()
          ; print ["val keywords ="]
          ; break ()
          ; ( case IM.toList keywords of
                nil => print ["  []"]
              | ( ( firstId , firstStr ) :: rest ) =>
                  ( print ["  [ (\"" , String.toString firstStr , "\" , " , Int.toString firstId , ")"]
                  ; List.app
                      (fn ( id , s ) =>
                        ( break ()
                        ; print ["  , (\"" , String.toString s , "\" , " , Int.toString id , ")"]
                        ))
                      rest
                  ; break ()
                  ; print ["  ]"]
                  )
            )
          ; closeBox pp
          ; break ()
          ; print [")"]
          ; break ()
          ; print ["open Internal"]
          ; break ()
          ; break ()
          ; List.app
              (fn name =>
                let
                  val moduleName = toModuleCase name
                  val proj =
                    if List.length terminalNames = 1
                    then String.concat ["(fn Terminal" , moduleName , " v => SOME v)"]
                    else String.concat ["(fn Terminal" , moduleName , " v => SOME v | _ => NONE)"]
                in
                  print ["val parseTerminal" , moduleName , " = parseTerminal " , proj , " \"" , name , "\""]
                  ; break ()
                end)
              terminalNames
          ; List.app
              (fn { name , ... } : I.definition =>
                ( print ["  val parse" , ntModule name , "Dummy : " , ntSnake name , " t_dummy = dummy ()"]
                ; break ()
                ))
              definitions
          ; break ()
          ; print ["val lex = lex"]
          ; break ()
          ; break ()
          ; List.app (emitParser "  ") definitions
          ; break ()
          ; List.app
              (fn { name , ... } : I.definition =>
                ( print ["val () = set parse" , ntModule name , "Dummy parse" , ntModule name]
                ; break ()
                ))
              definitions
          ; break ()
          ; print ["val parse = parse"]
        ; closeBox pp
        ; break ()
        ; break ()
        ; print ["end"]

        (* print functor *)
        ; break ()
        ; break ()
        ; openBox pp Vertical 2
        ; print ["functor " , toModuleCase name , "Print ("]
        ; break ()
        ; print ["structure Ast : " , sigName]
        ; break ()
        ; openBox pp Vertical 2
        ; print ["structure Terminals : sig"]
        ; List.app
            (fn name =>
              ( break ()
              ; print ["structure " , toModuleCase name , " : PRINT_TERMINAL where type t = Ast." , toSnakeCase name]
              ))
            terminalNames
        ; closeBox pp
        ; break ()
        ; print ["end"]
        ; closeBox pp
        ; break ()
        ; print [") :>"]
        ; break ()
        ; openBox pp Vertical 2
        ; print ["sig"]
        ; break ()
        ; break ()
        ; List.app
            (fn name =>
              ( print ["val print" , toModuleCase name , " : Ast." , toSnakeCase name , " Ast.annot -> string"]
              ; break ()
              ))
            terminalNames
        ; List.app
            (fn { id , ... } : { id : I.id , rules : { name : string , ty : I.ty list } list } =>
              ( print ["val print" , ntModule id , " : Ast." , ntSnake id , " -> string"]
              ; break ()
              ))
            datatypes
        ; break ()
        ; List.app
            (fn name =>
              ( print ["val prettyPrint" , toModuleCase name , " : Ast." , toSnakeCase name , " Ast.annot -> string"]
              ; break ()
              ))
            terminalNames
        ; List.app
            (fn { name , ... } : I.definition =>
              ( print ["val prettyPrint" , ntModule name , " : Ast." , ntSnake name , " -> string"]
              ; break ()
              ))
            definitions
        ; closeBox pp
        ; break ()
        ; print ["end = struct"]
        ; break ()
        ; openBox pp Vertical 2
          ; break ()
          ; print ["open Ast"]
          ; break ()
          ; break ()
          ; print ["val push = PrintBuffer.push"]
          ; break ()
          ; break ()
          ; let
              fun printCallForTy v ty =
                case ty of
                  I.TyTerminal tid =>
                    String.concat ["print" , tModule tid , " buf " , v]
                | I.TyNonterminal nid =>
                    String.concat ["print" , ntModule nid , " buf " , v]
                | I.TyList innerTy =>
                    let val inner = v ^ "e"
                    in String.concat
                      [ "( push buf \"[\" lineno"
                      , " ; List.appi (fn ( i , " , inner , " ) => "
                      , "( if i > 0 then push buf \" , \" lineno else ()"
                      , " ; " , printCallForTy inner innerTy
                      , " )) " , v
                      , " ; push buf \"]\" lineno )"
                      ]
                    end
                | I.TyOption innerTy =>
                    let val inner = v ^ "v"
                    in String.concat
                      [ "(case " , v , " of NONE => push buf \"_\" lineno"
                      , " | SOME " , inner , " => " , printCallForTy inner innerTy
                      , ")"
                      ]
                    end
                | I.TyTuple tys =>
                    let
                      val vars = List.mapi (fn ( i , _ ) => v ^ Int.toString i) tys
                      val prints = ListPair.map (fn ( vi , ty ) => printCallForTy vi ty) (vars , tys)
                    in
                      case prints of
                        nil => String.concat ["let val () = " , v , " in () end"]
                      | first :: rest =>
                          String.concat
                          [ "let val (" , String.concatWith " , " vars , ") = " , v
                          , " in push buf \"(\" lineno"
                          , " ; " , first
                          , String.concat (List.map (fn p => " ; push buf \" , \" lineno ; " ^ p) rest)
                          , " ; push buf \")\" lineno end"
                          ]
                    end

            in
              List.app
                (fn name =>
                  ( openBox pp Vertical 2
                  ; print ["fun print" , toModuleCase name , " buf"]
                  ; break ()
                  ; print ["( { node , span = { start = { lineno , ... } , ... } } : " , toSnakeCase name , " annot ) ="]
                  ; break ()
                  ; print ["push buf (Terminals." , toModuleCase name , ".show node) lineno"]
                  ; closeBox pp
                  ; break ()
                  ; break ()
                  ))
                terminalNames
              ; List.appi
                  (fn ( i , { id , rules } ) =>
                    let
                      val keyword = if i = 0 then "fun" else "and"
                      val defName = ntName id
                    in
                      openBox pp Vertical 2
                      ; print [keyword , " print" , ntModule id , " buf"]
                      ; break ()
                      ; print ["( { node , span = { start = { lineno , ... } , ... } } : " , ntSnake id , " ) ="]
                      ; break ()
                      ; openBox pp Vertical 2
                      ; print ["case node of"]
                      ; break ()
                      ; List.appi
                          (fn ( j , { name , ty } : { name : string , ty : I.ty list } ) =>
                            let
                              val conName = toModuleCase defName ^ name
                              val vars = List.mapi (fn ( i , _ ) => String.concat ["v" , Int.toString i]) ty
                              val pat =
                                case vars of
                                  nil => conName
                                | [v] => String.concat [conName , " " , v]
                                | _ => String.concat [conName , " (" , String.concatWith " , " vars , ")"]
                            in
                              if j > 0 then ( break () ; print ["| "] ) else print ["  "]
                              ; openBox pp Vertical 2
                              ; print [pat , " =>"]
                              ; break ()
                              ; print ["( push buf \"" , conName , "\" lineno"]
                              ; ( case ty of
                                    nil => ()
                                  | _ =>
                                      ( break ()
                                      ; print ["; push buf \"(\" lineno"]
                                      ; List.appi
                                          (fn ( i , ( v , t ) ) =>
                                            ( if i > 0
                                              then ( break ()
                                                   ; print ["; push buf \" , \" lineno"]
                                                   )
                                              else ()
                                            ; break ()
                                            ; print ["; " , printCallForTy v t]
                                            ))
                                          (ListPair.zip (vars , ty))
                                      ; break ()
                                      ; print ["; push buf \")\" lineno"]
                                      )
                                )
                              ; break ()
                              ; print [")"]
                              ; closeBox pp
                            end)
                          rules
                      ; closeBox pp
                      ; closeBox pp
                      ; break ()
                      ; break ()
                    end)
                  datatypes
              ; break ()
              ; print ["fun print f = fn v =>"]
              ; break ()
              ; print ["let val buf = PrintBuffer.empty ()"]
              ; break ()
              ; print ["in f buf v"]
              ; break ()
              ; print ["; PrintBuffer.toString buf"]
              ; break ()
              ; print ["end"]
              ; break ()
              ; List.app
                  (fn name =>
                    ( print ["val print" , toModuleCase name , " = print print" , toModuleCase name]
                    ; break ()
                    ))
                  terminalNames
              ; List.app
                  (fn { id , ... } : { id : I.id , rules : { name : string , ty : I.ty list } list } =>
                    ( print ["val print" , ntModule id , " = print print" , ntModule id]
                    ; break ()
                    ))
                  datatypes
              ; break ()
              ; break ()
              ; let
                  fun findArgs cmd =
                    case cmd of
                      I.Return { args , allVars = _ } => args
                    | I.Bind { andthen , ... } => findArgs andthen

                  fun subCmdPat subCmd =
                    case findArgs subCmd of
                      nil => "_"
                    | [a] => varName a
                    | args => String.concat ["(" , String.concatWith " , " (List.map varName args) , ")"]

                  fun emitPrettyPrintCmd isFirst allArgs cmd =
                    case cmd of
                      I.Return _ => ()
                    | I.Bind { var , parser , andthen } =>
                        case parser of
                          I.Keyword _ =>
                            ( if isFirst then () else ( break () ; print ["; "] )
                            ; emitPrettyPrintBind var allArgs parser
                            ; emitPrettyPrintCmd false allArgs andthen
                            )
                        | _ =>
                            if List.exists (fn a => a = var) allArgs
                            then
                              ( if isFirst then () else ( break () ; print ["; "] )
                              ; emitPrettyPrintBind var allArgs parser
                              ; emitPrettyPrintCmd false allArgs andthen
                              )
                            else emitPrettyPrintCmd isFirst allArgs andthen

                  and emitPrettyPrintBind var allArgs parser =
                    let val v = varName var
                    in
                      case parser of
                        I.Keyword kid =>
                          print ["push buf \"" , String.toString (nameOfId keywords kid) , "\" lineno"]
                      | I.Terminal tid =>
                          print ["prettyPrint" , tModule tid , " buf " , v]
                      | I.Ref I.Self =>
                          print ["prettyPrintSelf buf " , v]
                      | I.Ref I.Higher =>
                          print ["prettyPrintSelf buf " , v]
                      | I.Ref (I.Other nid) =>
                          print ["prettyPrint" , ntModule nid , " buf " , v]
                      | I.Star subCmd =>
                          ( openBox pp Vertical 2
                          ; print ["List.app (fn " , subCmdPat subCmd , " =>"]
                          ; break ()
                          ; print ["( "]
                          ; emitPrettyPrintCmd true (findArgs subCmd) subCmd
                          ; print ["))"]
                          ; closeBox pp
                          ; print [" " , v]
                          )
                      | I.Plus subCmd =>
                          ( openBox pp Vertical 2
                          ; print ["List.app (fn " , subCmdPat subCmd , " =>"]
                          ; break ()
                          ; print ["( "]
                          ; emitPrettyPrintCmd true (findArgs subCmd) subCmd
                          ; print ["))"]
                          ; closeBox pp
                          ; print [" " , v]
                          )
                      | I.Opt subCmd =>
                          ( openBox pp Vertical 2
                          ; print ["(case " , v , " of NONE => ()"]
                          ; break ()
                          ; print ["| SOME " , subCmdPat subCmd , " =>"]
                          ; break ()
                          ; print ["( "]
                          ; emitPrettyPrintCmd true (findArgs subCmd) subCmd
                          ; print ["))"]
                          ; closeBox pp
                          )
                      | I.Seq subCmd =>
                          ( openBox pp Vertical 2
                          ; print ["let val " , subCmdPat subCmd , " = " , v]
                          ; break ()
                          ; print ["in "]
                          ; emitPrettyPrintCmd true (findArgs subCmd) subCmd
                          ; break ()
                          ; print ["end"]
                          ; closeBox pp
                          )
                    end
                in
                  List.app
                    (fn name =>
                      ( openBox pp Vertical 2
                      ; print ["fun prettyPrint" , toModuleCase name , " buf"]
                      ; break ()
                      ; print ["( { node , span = { start = { lineno , ... } , ... } } : " , toSnakeCase name , " annot ) ="]
                      ; break ()
                      ; print ["push buf (Terminals." , toModuleCase name , ".show node) lineno"]
                      ; closeBox pp
                      ; break ()
                      ; break ()
                      ))
                    terminalNames
                  ; List.appi
                      (fn ( i , { name = defId , atoms , levels } : I.definition ) =>
                        let
                          val keyword = if i = 0 then "fun" else "and"
                          val allRules =
                            atoms @ List.concatMap (fn { rules , ... } : I.level => rules) levels
                        in
                          openBox pp Vertical 2
                          ; print [keyword , " prettyPrint" , ntModule defId , " buf"]
                          ; break ()
                          ; print ["( { node , span = { start = { lineno , ... } , ... } } : " , ntSnake defId , " ) ="]
                          ; break ()
                          ; print ["let val prettyPrintSelf = prettyPrint" , ntModule defId]
                          ; break ()
                          ; print ["in"]
                          ; break ()
                          ; openBox pp Vertical 2
                          ; print ["case node of"]
                          ; break ()
                          ; List.appi
                              (fn ( j , { name , cmd } : I.rule ) =>
                                let
                                  val conName = String.concat [ntModule defId , name]
                                  val args = findArgs cmd
                                  val patVars = List.map varName args
                                  val pat =
                                    case patVars of
                                      nil => conName
                                    | [v] => String.concat [conName , " " , v]
                                    | _ => String.concat [conName , " (" , String.concatWith " , " patVars , ")"]
                                in
                                  if j > 0 then ( break () ; print ["| "] ) else print ["  "]
                                  ; openBox pp Vertical 2
                                  ; print [pat , " =>"]
                                  ; break ()
                                  ; print ["( "]
                                  ; emitPrettyPrintCmd true args cmd
                                  ; print [")"]
                                  ; closeBox pp
                                end)
                              allRules
                          ; closeBox pp
                          ; break ()
                          ; print ["end"]
                          ; closeBox pp
                          ; break ()
                          ; break ()
                        end)
                      definitions
                  ; break ()
                  ; List.app
                      (fn name =>
                        ( print ["val prettyPrint" , toModuleCase name , " = print prettyPrint" , toModuleCase name]
                        ; break ()
                        ))
                      terminalNames
                  ; List.app
                      (fn { name , ... } : I.definition =>
                        ( print ["val prettyPrint" , ntModule name , " = print prettyPrint" , ntModule name]
                        ; break ()
                        ))
                      definitions
                end
            end
        ; closeBox pp
        ; break ()
        ; print ["end"]

        (* repl functor *)
        ; break ()
        ; break ()
        ; openBox pp Vertical 2
        ; print ["functor " , toModuleCase name , "Repl ("]
        ; break ()
        ; print ["structure Trivial : " , terminalWhere]
        ; break ()
        ; openBox pp Vertical 2
        ; print ["structure Terminals : sig"]
        ; List.app
            (fn name =>
              ( break ()
              ; print ["structure " , toModuleCase name , " : REPL_TERMINAL"]
              ))
            terminalNames
        ; closeBox pp
        ; break ()
        ; print ["end"]
        ; closeBox pp
        ; break ()
        ; print [") :> sig"]
        ; break ()
        ; print ["  val run : unit -> unit"] 
        ; break ()
        ; print ["end = struct"]
        ; break ()
        ; openBox pp Vertical 2
          ; break ()
          ; openBox pp Vertical 2
          ; print ["structure Parser = " , toModuleCase name , "Parser ("]
          ; break ()
          ; print ["structure Trivial = Trivial"]
          ; break ()
          ; print ["structure Terminals = Terminals"]
          ; closeBox pp
          ; break ()
          ; print [")"]
          ; break ()
          ; break ()
          ; openBox pp Vertical 2
          ; print ["structure Print = " , toModuleCase name , "Print ("]
          ; break ()
          ; print ["structure Ast = Parser"]
          ; break ()
          ; print ["structure Terminals = Terminals"]
          ; closeBox pp
          ; break ()
          ; print [")"]
          ; break ()
          ; break ()
          ; let
              val { name = defId , ... } : I.definition = List.last definitions
            in
              openBox pp Vertical 2
              ; print ["structure Repl = Repl ("]
              ; break ()
              ; openBox pp Vertical 2
              ; print ["structure Result = struct"]
              ; break ()
              ; print ["open Parser"]
              ; break ()
              ; print ["type token_stream = TokenStream.t"]
              ; break ()
              ; print ["type t = " , ntSnake defId]
              ; break ()
              ; print ["val parse = parse parse" , ntModule defId]
              ; break ()
              ; print ["val print = Print.print" , ntModule defId]
              ; closeBox pp
              ; break ()
              ; print ["end"]
              ; closeBox pp
              ; break ()
              ; print [")"]
              ; break ()
              ; break ()
              ; print ["val run = Repl.run"]
            end
        ; closeBox pp
        ; break ()
        ; print ["end"]

        ; closeBox pp
        ; flush pp
        ; TextIO.closeOut out
      end

  end
