structure Grammar =
  struct

    type id = int

    structure NameMap = SplayDict (structure Key = StringOrdered)

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

    datatype rule
      = Seq of rule list
      | Star of rule
      | Opt of rule
      | Terminal of id
      | Keyword of string
      | Nonterminal of id

    type alt =
      { rule : rule
      , ruleName : string
      , fixity : fixity
      , precedence : precedence
      }

    type definition = { name : id , alts : alt list }

    type grammar =
      { nonterminalMap : id NameMap.dict
      , terminalMap : id NameMap.dict
      , definitions : definition list
      , keywords : string list
      }

  end
