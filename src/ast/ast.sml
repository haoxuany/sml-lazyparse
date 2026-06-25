structure Ast =
  struct

    type id = string

    datatype assoc
      = Left
      | Right
      | None

    datatype property
      = Assoc of assoc
      | Prec of int
      | RuleName of id

    datatype rule
      = Seq of rule list               (* A B C *)
      | Star of rule                   (* A* *)
      | Plus of rule                   (* A+ *)
      | Opt of rule                    (* A? *)
      | Terminal of string             (* 'terminal' *)
      | Keyword of string              (* "terminal" *)
      | Nonterminal of id              (* A *)

    (* Name ::= rule [ name Plus , left 3 ] | rule | ... *)
    type definition = { name : id , rule : (rule * property list) list }

    type grammar = { definitions : definition list }

  end
