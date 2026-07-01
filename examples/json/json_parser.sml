
structure Run = struct

  local
    structure R = Regex

    val jsonNumber =
      let
        val digits = R.plus R.digit
        val sign = R.opt (R.exact #"-")
        val frac = R.seq [R.exact #"." , digits]
        val expo = R.seq [R.set [#"e" , #"E"] , R.opt (R.set [#"+" , #"-"]) , digits]
      in
        R.seq [sign , digits , R.opt frac , R.opt expo]
      end
  in
    structure JsonRepl = JsonRepl (
      structure Trivial = LexCommon.WhitespaceTrivial (
        val whitespace = [" " , "\t" , "\n" , "\r"]
      )
      structure Terminals = struct
        structure Number = LexCommon.RegexReplTerminal (
          type t = string
          val regex = jsonNumber
          fun map s = s
          fun show s = s
        )
        structure String = LexCommon.StringTerminal
      end
    )
  end

  val run = JsonRepl.run
end
