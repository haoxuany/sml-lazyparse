
structure Run = struct

  structure CalcRepl = CalcRepl (
    structure Trivial = LexCommon.WhitespaceTrivial (
      val whitespace = [" " , "\t" , "\n" , "\r"]
    )
    structure Terminals = struct
      structure Number = LexCommon.RegexReplTerminal (
        type t = string
        val regex = Regex.seq [Regex.opt (Regex.exact #"-") , Regex.plus Regex.digit]
        fun map s = s
        fun show s = s
      )
    end
  )

  val _ = CalcRepl.run ()
end
