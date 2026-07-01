
structure Run = struct

  local
    structure R = Regex
  in
    structure ArithRepl = ArithRepl (
      structure Trivial = LexCommon.WhitespaceTrivial (
        val whitespace = [" " , "\t" , "\n" , "\r"]
      )
      structure Terminals = struct
        structure Nat = LexCommon.RegexReplTerminal (
          type t = int
          val regex = R.plus R.digit
          fun map s =
            case Int.fromString s of
              NONE => raise Fail "Impossible"
            | SOME v => v
          val show = Int.toString
        )
      end
    )
  end

  val run = ArithRepl.run
end
