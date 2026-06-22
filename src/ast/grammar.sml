structure Grammar =
  struct

    type id = int

    datatype assoc
      = Left
      | Right
      | None

    type precedence = int

    datatype fixity
      = Infix of assoc
      | Prefix
      | Postfix
      | Nonfix

    datatype spec
      = Seq of spec list
      | Star of spec
      | Plus of spec
      | Opt of spec
      | Terminal of id
      | Keyword of id
      | Nonterminal of id

    type rule =
      { spec : spec
      , name : string
      , fixity : fixity
      , precedence : precedence
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
