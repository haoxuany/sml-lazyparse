structure IL =
  struct

    type id = Grammar.id

    datatype ref_kind
      = Self
      | Higher
      | Other of id

    datatype spec
      = Seq of spec list
      | Star of spec
      | Plus of spec
      | Opt of spec
      | Terminal of id
      | Keyword of id
      | Ref of ref_kind

    type rule =
      { name : string
      , spec : spec
      }

    type level =
      { precedence : int
      , rules : rule list
      }

    type definition =
      { name : id
      , atoms : rule list
      , levels : level list
      }

    structure IdMap = Grammar.IdMap

    type t =
      { nonterminals : string IdMap.dict
      , terminals : string IdMap.dict
      , keywords : string IdMap.dict
      , definitions : definition list
      }

  end
