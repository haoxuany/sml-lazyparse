structure IL =
  struct

    type id = Grammar.id
    type var = int

    datatype ref_kind
      = Self
      | Higher
      | Other of id

    datatype 'a parser
      = Terminal of id
      | Keyword of id
      | Ref of ref_kind
      | Star of 'a cmd
      | Plus of 'a cmd
      | Opt of 'a cmd
      | Seq of 'a cmd

    and 'a cmd =
      Bind of { var : var , parser : 'a parser , andthen : 'a cmd }
    | Return of 'a

    type ret =
      { args : var list
      , allVars : var list
      }

    type rule =
      { name : string
      , cmd : ret cmd
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

    datatype ty
      = TyTerminal of id
      | TyNonterminal of id
      | TyList of ty
      | TyOption of ty
      | TyTuple of ty list

    structure IdMap = Grammar.IdMap

    type t =
      { nonterminals : string IdMap.dict
      , terminals : string IdMap.dict
      , keywords : string IdMap.dict
      , datatypes :
        { id : id
        , rules : { name : string , ty : ty list } list
        } list
      , definitions : definition list
      }

  end
