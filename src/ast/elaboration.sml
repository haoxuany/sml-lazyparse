structure Elaboration :
  sig
    exception UnboundNonterminal of string
    exception MissingRuleName
    exception DuplicateRuleName
    val elaborate : Ast.grammar -> Grammar.grammar
  end =
  struct

    exception UnboundNonterminal of string
    exception MissingRuleName
    exception DuplicateRuleName

    structure NM = Grammar.NameMap

    structure StringSet = SplaySet (structure Elem = StringOrdered)

    fun elaborate { definitions = astDefs } =
      let
        val nonterminalMap =
          List.foldl
            (fn ((id , name) , nm) => NM.insert nm name id)
            NM.empty
            (List.mapi (fn (i , { name , ... } : Ast.definition) => (i , name)) astDefs)

        val nextTermId = ref 0
        val terminalMap = ref NM.empty
        val keywordSet = ref StringSet.empty

        fun lookupName name =
          case NM.find nonterminalMap name
            of SOME id => id
             | NONE => raise UnboundNonterminal name

        fun lookupTerminal name =
          case NM.find (!terminalMap) name
            of SOME id => id
             | NONE =>
                 let val id = !nextTermId
                 in nextTermId := id + 1
                  ; terminalMap := NM.insert (!terminalMap) name id
                  ; id
                 end

        fun elabRule (Ast.Seq rs) = Grammar.Seq (List.map elabRule rs)
          | elabRule (Ast.Star r) = Grammar.Star (elabRule r)
          | elabRule (Ast.Opt r) = Grammar.Opt (elabRule r)
          | elabRule (Ast.Terminal s) = Grammar.Terminal (lookupTerminal s)
          | elabRule (Ast.Keyword s) =
              ( keywordSet := StringSet.insert (!keywordSet) s
              ; Grammar.Keyword s
              )
          | elabRule (Ast.Nonterminal name) = Grammar.Nonterminal (lookupName name)

        fun elabAssoc Ast.Left = Grammar.Left
          | elabAssoc Ast.Right = Grammar.Right
          | elabAssoc Ast.None = Grammar.None

        fun extractRuleName props =
          let
            val names = List.mapPartial
              (fn Ast.RuleName s => SOME s | _ => NONE) props
            val rest = List.filter
              (fn Ast.RuleName _ => false | _ => true) props
          in
            case names
              of [n] => (n , rest)
               | nil => raise MissingRuleName
               | _ => raise DuplicateRuleName
          end

        fun extractAssoc props =
          let
            val assocs = List.mapPartial
              (fn Ast.Assoc a => SOME a | _ => NONE) props
            val precs = List.mapPartial
              (fn Ast.Prec n => SOME n | _ => NONE) props
            val assoc =
              case assocs of
                nil => Grammar.Left
              | [a] => elabAssoc a
              | _ => raise Fail "multiple assoc annotations"
            val precedence =
              case precs of
                nil => 5
              | [n] => n
              | _ => raise Fail "multiple prec annotations"
          in
            (assoc , precedence)
          end

        fun flattenRule (Grammar.Seq rs) = List.concatMap flattenRule rs
          | flattenRule r = [r]

        fun isSelfRef defId (Grammar.Nonterminal id) = id = defId
          | isSelfRef _ _ = false

        fun classifyFixity defId rule assoc =
          let
            val flat = flattenRule rule
          in
            case flat
              of nil => Grammar.Nonfix
               | _ =>
                   let
                     val first = hd flat
                     val last = List.last flat
                     val startsWithSelf = isSelfRef defId first
                     val endsWithSelf = isSelfRef defId last
                   in
                     case (startsWithSelf , endsWithSelf)
                       of (true , true) => Grammar.Infix assoc
                        | (true , false) => Grammar.Postfix
                        | (false , true) => Grammar.Prefix
                        | (false , false) => Grammar.Nonfix
                   end
          end

        fun elabAlt defId (r , props) =
          let
            val (ruleName , rest) = extractRuleName props
            val elabedRule = elabRule r
            val (assoc , precedence) = extractAssoc rest
            val fixity = classifyFixity defId elabedRule assoc
          in
            { rule = elabedRule
            , ruleName = ruleName
            , fixity = fixity
            , precedence = precedence
            }
          end

        fun elabDef { name , rule } =
          let val defId = lookupName name
          in { name = defId , alts = List.map (elabAlt defId) rule }
          end

        val elabedDefs = List.map elabDef astDefs
      in
        { nonterminalMap = nonterminalMap
        , terminalMap = !terminalMap
        , definitions = elabedDefs
        , keywords = StringSet.toList (!keywordSet)
        }
      end

  end
