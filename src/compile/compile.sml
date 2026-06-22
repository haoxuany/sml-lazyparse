structure Compile :
  sig
    val compile : Grammar.grammar -> IL.t
  end =
  struct

    structure G = Grammar
    structure I = IL

    fun compileSpec defId spec =
      case spec of
        G.Seq rs => I.Seq (List.map (compileSpec defId) rs)
      | G.Star r => I.Star (compileSpec defId r)
      | G.Plus r => I.Plus (compileSpec defId r)
      | G.Opt r => I.Opt (compileSpec defId r)
      | G.Terminal id => I.Terminal id
      | G.Keyword id => I.Keyword id
      | G.Nonterminal id =>
          if id = defId
          then I.Ref I.Self
          else I.Ref (I.Other id)

    fun flattenSpec r =
      case r of
        I.Seq rs => List.concatMap flattenSpec rs
      | _ => [r]

    fun compileRule defId ({ name , spec , ... } : G.rule) =
      { name = name
      , spec = compileSpec defId spec
      }

    structure IntMap = SplayDict (structure Key = IntOrdered)

    fun compileDef ({ name = defId , rules } : G.definition) =
      let
        val (nonfixRules , fixityRules) =
          List.partition
            (fn ({ fixity , ... } : G.rule) =>
              case fixity of
                G.Nonfix => true
              | _ => false)
            rules

        val atoms = List.map (compileRule defId) nonfixRules

        fun isLeftRecursive ({ fixity , ... } : G.rule) =
          case fixity of
            G.Postfix => true
          | G.Infix G.Left => true
          | _ => false

        val precGroups =
          List.foldl
            (fn (rule as { precedence , ... } : G.rule , m) =>
              IntMap.insertMerge m precedence [rule] (fn l => l @ [rule]))
            IntMap.empty
            fixityRules

        val levels =
          List.map
            (fn (prec , group) =>
              let
                val compiled =
                  List.map
                    (fn rule =>
                      let
                        val { name , spec } = compileRule defId rule
                        val flat = flattenSpec spec
                        val isLeft = isLeftRecursive rule
                        val last = List.length flat - 1

                        fun resolve (i , elem) =
                          case elem of
                            I.Ref I.Self =>
                              if i = 0 andalso isLeft then I.Ref I.Self
                              else if i = last andalso not isLeft then I.Ref I.Self
                              else I.Ref I.Higher
                          | I.Seq rs => I.Seq (List.mapi resolve rs)
                          | I.Star r => I.Star (resolve (0 , r))
                          | I.Plus r => I.Plus (resolve (0 , r))
                          | I.Opt r => I.Opt (resolve (0 , r))
                          | _ => elem
                      in
                        { name = name
                        , spec = I.Seq (List.mapi resolve flat)
                        }
                      end)
                    group
              in
                { precedence = prec , rules = compiled }
              end)
            (List.rev (IntMap.toList precGroups))
      in
        { name = defId
        , atoms = atoms
        , levels = levels
        }
      end

    fun compile ({ nonterminals , terminals , definitions , keywords } : G.grammar) =
      { nonterminals = nonterminals
      , terminals = terminals
      , keywords = keywords
      , definitions = List.map compileDef definitions
      }

  end
