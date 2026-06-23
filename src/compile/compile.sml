structure Compile :
  sig
    val compile : Grammar.grammar -> IL.t
  end =
  struct

    structure G = Grammar
    structure I = IL
    structure IntMap = SplayDict (structure Key = IntOrdered)

    datatype fixity =
      Infix of G.inner list * G.assoc
    | Prefix of G.inner list
    | Postfix of G.inner list
    | Nonfix of G.inner list

    fun compile ({ nonterminals , terminals , definitions , keywords } : G.grammar) =
      let
        val datatypes =
          let
            fun ruleTypes inner =
              let
                fun wrap f tys =
                  case tys of
                    nil => nil
                  | [t] => [f t]
                  | ts => [f (I.TyTuple ts)]
              in
                case inner of
                  G.Seq rs => wrap (fn x => x) (List.concatMap ruleTypes rs)
                | G.Star r => wrap I.TyList (ruleTypes r)
                | G.Plus r => wrap I.TyList (ruleTypes r)
                | G.Opt r => wrap I.TyOption (ruleTypes r)
                | G.Terminal id => [I.TyTerminal id]
                | G.Keyword _ => nil
                | G.Nonterminal id => [I.TyNonterminal id]
              end

            fun specTypes defName spec =
              case spec of
                G.Nonfix inners => List.concatMap ruleTypes inners
              | G.Infix ( inners , _ , _ ) =>
                  List.concat
                  [ [I.TyNonterminal defName]
                  , List.concatMap ruleTypes inners
                  , [I.TyNonterminal defName]
                  ]
              | G.Prefix ( inners , _ ) =>
                  List.concatMap ruleTypes inners
                  @ [I.TyNonterminal defName]
              | G.Postfix ( inners , _ ) =>
                  ( I.TyNonterminal defName ) :: ( List.concatMap ruleTypes inners )
          in
            List.map
              (fn { name , rules } =>
                { id = name
                , rules =
                    List.map
                      (fn { name = ruleName , spec } : G.rule =>
                        { name = ruleName , ty = specTypes name spec })
                      rules
                })
              definitions
          end

        val definitions =
          List.map
            (fn ( { name , rules } : G.definition ) =>
              let
                fun compileInners idx args allVars inners cont =
                  case inners of
                    nil => cont ( idx , args , allVars )
                  | inner :: rest =>
                      compileBind idx args allVars inner (fn ( idx , args , allVars ) =>
                        compileInners idx args allVars rest cont)

                and compileBind idx args allVars inner cont =
                  let
                    fun sub wrap inners =
                      let
                        val subCmd =
                          compileInners (idx + 1) nil nil inners (fn ( _ , args , allVars ) =>
                            I.Return { args = List.rev args
                                     , allVars = List.rev allVars })
                      in
                        I.Bind { var = idx , parser = wrap subCmd
                               , andthen = cont ( idx + 1 , idx :: args , idx :: allVars ) }
                      end
                  in
                    case inner of
                      G.Keyword id =>
                        I.Bind { var = idx , parser = I.Keyword id
                               , andthen = cont ( idx + 1 , args , idx :: allVars ) }
                    | G.Terminal id =>
                        I.Bind { var = idx , parser = I.Terminal id
                               , andthen = cont ( idx + 1 , idx :: args , idx :: allVars ) }
                    | G.Nonterminal id =>
                        I.Bind { var = idx , parser = I.Ref (I.Other id)
                               , andthen = cont ( idx + 1 , idx :: args , idx :: allVars ) }
                    | G.Star r => sub I.Star [r]
                    | G.Plus r => sub I.Plus [r]
                    | G.Opt r => sub I.Opt [r]
                    | G.Seq rs => sub I.Seq rs
                  end

                fun compileRule { name = ruleName , fixity } =
                  let
                    val cmd =
                      case fixity of
                        Nonfix inners =>
                          compileInners 0 nil nil inners (fn ( _ , args , allVars ) =>
                            I.Return { args = List.rev args
                                     , allVars = List.rev allVars })
                      | Infix ( inners , assoc ) =>
                          let
                            val ( leftRef , rightRef ) =
                              case assoc of
                                G.Left => ( I.Self , I.Higher )
                              | G.Right => ( I.Higher , I.Self )
                              | G.None => ( I.Higher , I.Higher )
                          in
                            I.Bind { var = 0 , parser = I.Ref leftRef
                                   , andthen =
                              compileInners 1 [0] [0] inners (fn ( idx , args , allVars ) =>
                                I.Bind { var = idx , parser = I.Ref rightRef
                                       , andthen =
                                  I.Return { args = List.rev (idx :: args)
                                           , allVars = List.rev (idx :: allVars) } }) }
                          end
                      | Prefix inners =>
                          compileInners 0 nil nil inners (fn ( idx , args , allVars ) =>
                            I.Bind { var = idx , parser = I.Ref I.Self
                                   , andthen =
                              I.Return { args = List.rev (idx :: args)
                                       , allVars = List.rev (idx :: allVars) } })
                      | Postfix inners =>
                          I.Bind { var = 0 , parser = I.Ref I.Self
                                 , andthen =
                            compileInners 1 [0] [0] inners (fn ( _ , args , allVars ) =>
                              I.Return { args = List.rev args
                                       , allVars = List.rev allVars }) }
                  in
                    { name = ruleName
                    , cmd = cmd
                    }
                  end

                val ( nonfixRules , fixityRules ) =
                  List.foldl
                    (fn ( { spec , name } : G.rule , ( nonfixRules , fixityRules ) ) =>
                      let
                        fun insert p fixity =
                          ( nonfixRules
                          , IntMap.insertMerge fixityRules p
                              [{ name = name , fixity = fixity }]
                              (fn l => { name = name , fixity = fixity } :: l)
                          )
                      in
                        case spec of
                          G.Infix ( inner , assoc , p ) =>
                            insert p ( Infix ( inner , assoc ) )
                        | G.Prefix ( inner , p ) =>
                            insert p ( Prefix inner )
                        | G.Postfix ( inner , p ) =>
                            insert p ( Postfix inner )
                        | G.Nonfix inner =>
                            ( { name = name , fixity = Nonfix inner } :: nonfixRules
                            , fixityRules
                            )
                      end)
                    ( nil , IntMap.empty )
                    rules

                val atoms = List.map compileRule nonfixRules

                val levels =
                  List.map
                    (fn ( prec , group ) =>
                      { precedence = prec
                      , rules = List.map compileRule group
                      })
                    (List.rev (IntMap.toList fixityRules))
              in
                { name = name
                , atoms = atoms
                , levels = levels
                }
              end)
            definitions

      in
        { nonterminals = nonterminals
        , terminals = terminals
        , keywords = keywords
        , datatypes = datatypes
        , definitions = definitions
        }
      end

  end
