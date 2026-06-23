structure Grammar =
  struct

    type id = int

    datatype assoc
      = Left
      | Right
      | None

    type precedence = int

    datatype inner
      = Seq of inner list
      | Star of inner
      | Plus of inner
      | Opt of inner
      | Terminal of id
      | Keyword of id
      | Nonterminal of id

    datatype spec
      = Infix of inner list * assoc * precedence
      | Prefix of inner list * precedence
      | Postfix of inner list * precedence
      | Nonfix of inner list

    type rule =
      { spec : spec
      , name : string
      }

    type definition = { name : id , rules : rule list }

    structure IdMap = SplayDict (structure Key = IntOrdered)

    type grammar =
      { nonterminals : string IdMap.dict
      , terminals : string IdMap.dict
      , keywords : string IdMap.dict
      , definitions : definition list
      }

  end
