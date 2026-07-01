structure Elaboration :
  sig
    exception UnboundNonterminal of string
    exception MissingRuleName
    exception DuplicateProperty of string
    exception InvalidAssociativity of string

    val elaborate : Ast.grammar -> Grammar.grammar
  end =
  struct

    exception UnboundNonterminal of string
    exception MissingRuleName
    exception DuplicateProperty of string
    exception InvalidAssociativity of string

    local

      structure NameIdDict = SplayDict (structure Key = StringOrdered)
      structure IdNameDict = Grammar.IdMap

    in

      fun buildTwoSided (l : (int * string) list) =
        let
          val nameToId =
            List.foldl
              (fn ((id , name) , nm) => NameIdDict.insert nm name id)
              NameIdDict.empty
              l

          fun lookupName name =
            case NameIdDict.find nameToId name of
              SOME v => v
            | NONE => raise UnboundNonterminal name

          val idToName =
            List.foldl
              (fn ((id , name) , nm) => IdNameDict.insert nm id name)
              IdNameDict.empty
              l
        in
          { lookupName = lookupName
          , idMap = idToName
          }
        end

    end

    structure StringSet = SplaySet (structure Elem = StringOrdered)
    structure A = Ast
    structure G = Grammar

    fun elaborate { definitions } =
      let

        val { lookupName = lookupNonterminal , idMap = nonterminals } =
          buildTwoSided
            (List.mapi
              (fn (i , { name , ... } : A.definition) => (i , name))
              definitions)

        (* Pass 1: collect all terminal names and keywords *)
        val (terminals , keywords) =
          let
            fun collectSpec (r , (terms , kws)) =
              case r of
                A.Seq rs => List.foldl collectSpec (terms , kws) rs
              | A.Star r => collectSpec (r , (terms , kws))
              | A.Plus r => collectSpec (r , (terms , kws))
              | A.Opt r => collectSpec (r , (terms , kws))
              | A.Terminal s => (StringSet.insert terms s , kws)
              | A.Keyword s => (terms , StringSet.insert kws s)
              | A.Nonterminal _ => (terms , kws)
          in
            List.foldl
              (fn ({ rule , ... } : A.definition , acc) =>
                List.foldl (fn ((r , _) , acc) => collectSpec (r , acc)) acc rule)
              (StringSet.empty , StringSet.empty)
              definitions
          end

        val { lookupName = lookupTerminal , idMap = terminals } =
          buildTwoSided (List.mapi (fn v => v) (StringSet.toList terminals))

        val { lookupName = lookupKeyword , idMap = keywords } =
          buildTwoSided (List.mapi (fn v => v) (StringSet.toList keywords))

        (* Pass 2: elaborate *)
        fun elabRule defName (spec , props) =
          let
            (* extract properties *)
            val name = ref NONE
            val assoc = ref NONE
            val prec = ref NONE

            fun set (r , v , prop) =
              case !r of
                SOME _ => raise DuplicateProperty prop
              | NONE => r := SOME v

            val () =
              List.app
                (fn A.RuleName n => set (name , n , "name")
                  | A.Assoc a =>
                      set (assoc ,
                        case a of
                          A.Left => G.Left
                        | A.Right => G.Right
                        | A.None => G.None ,
                        "assoc")
                  | A.Prec n => set (prec , n , "prec"))
                props

            val name =
              case !name of
                SOME n => n
              | NONE => raise MissingRuleName
            val assoc = !assoc
            val precedence = !prec

            fun elabSpec r =
              case r of
                A.Seq rs => G.Seq (List.map elabSpec rs)
              | A.Star r => G.Star (elabSpec r)
              | A.Plus r => G.Plus (elabSpec r)
              | A.Opt r => G.Opt (elabSpec r)
              | A.Terminal s => G.Terminal (lookupTerminal s)
              | A.Keyword s => G.Keyword (lookupKeyword s)
              | A.Nonterminal name => G.Nonterminal (lookupNonterminal name)
            val spec = elabSpec spec

            fun flatten r =
              case r of
                G.Seq [ single ] => flatten single
              | G.Seq r => G.Seq (List.map flatten r)
              | G.Star r => G.Star (flatten r)
              | G.Plus r => G.Plus (flatten r)
              | G.Opt r => G.Opt (flatten r)
              | _ => r

            val spec =
              case flatten spec of
                G.Seq r => r
              | r => [ r ]

            val spec =
              let
                val defaultPrec = 50

                fun nonfix' () =
                  let 
                    val () =
                      case assoc of 
                        SOME _ =>
                          raise InvalidAssociativity
                            (String.concat
                              ["rule '" , name , "' has associativity but is nonfix (not infix)"])
                      | NONE => ()
                    val () =
                      case precedence of 
                        SOME _ =>
                          raise InvalidAssociativity
                            (String.concat
                              ["rule '" , name , "' is nonfix so precedence rules don't apply"])
                      | NONE => ()
                  in
                    G.Nonfix spec
                  end

                fun infix' inner =
                  let 
                    val assoc =
                      case assoc of 
                        SOME v => v
                      | NONE => G.Left
                    val precedence =
                      case precedence of 
                        SOME v => v
                      | NONE => defaultPrec
                  in
                    G.Infix ( inner , assoc , precedence )
                  end

                fun prefix' inner =
                  let 
                    val () =
                      case assoc of 
                        SOME _ =>
                          raise InvalidAssociativity
                            (String.concat
                              ["rule '" , name , "' has associativity but is prefix (not infix)"])
                      | NONE => ()
                    val precedence =
                      case precedence of 
                        SOME v => v
                      | NONE => defaultPrec
                  in
                    G.Prefix ( inner , precedence )
                  end

                fun postfix' inner = 
                  let 
                    val () =
                      case assoc of 
                        SOME _ =>
                          raise InvalidAssociativity
                            (String.concat
                              ["rule '" , name , "' has associativity but is postfix (not infix)"])
                      | NONE => ()
                    val precedence =
                      case precedence of 
                        SOME v => v
                      | NONE => defaultPrec
                  in
                    G.Postfix ( inner , precedence )
                  end
              in
                case spec of
                  nil =>  nonfix' ()
                | [ single ] => nonfix' ()
                | hd :: tl =>
                    ( case ( hd , List.rev tl ) of
                        ( _ , nil ) => raise Fail "Impossible"
                      | ( G.Nonterminal id , ( G.Nonterminal id2 ) :: rest ) =>
                          ( case ( id = defName , id2 = defName ) of
                               ( true , true ) =>
                                 infix' (List.rev rest)
                            | ( true , false ) =>
                                postfix' tl
                            | ( false , true ) =>
                                prefix' ( hd :: ( List.rev rest ) )
                            | ( false , false ) =>
                                nonfix' ()
                          )
                      | ( G.Nonterminal id , _ ) =>
                          if id = defName then
                            postfix' tl
                          else 
                            nonfix' ()
                      | ( _ , ( G.Nonterminal id2 ) :: rest ) =>
                          if id2 = defName then
                            prefix' ( hd :: ( List.rev rest ) ) 
                          else
                            nonfix' ()
                      | _ => 
                          nonfix' ()
                    )
              end
          in
            { spec = spec
            , name = name
            }
          end

        val definitions =
          List.map
            (fn { name , rule } =>
              let
                val name = lookupNonterminal name
              in
                { name = name , rules = List.map (elabRule name) rule }
              end)
            definitions
      in
        { nonterminals = nonterminals
        , terminals = terminals
        , definitions = definitions
        , keywords = keywords
        }
      end

  end
