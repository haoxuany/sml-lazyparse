
structure LexCommon = struct

  structure R = Regex
  structure LS = LexStream

  functor RegexTerminal (
    type t
    val regex : R.t
    val map : string -> t
  ) : TERMINAL = struct
    type t = t

    fun lex ts =
      case R.regex regex ts of
        SOME ( v , ts' ) => SOME ( map v , ts')
      | NONE => NONE
  end

  functor RegexReplTerminal (
    type t
    val regex : R.t
    val map : string -> t
    val show : t -> string
  ) : REPL_TERMINAL = struct
    structure T = RegexTerminal (
      type t = t
      val regex = regex
      val map = map
    )
    open T
    val show = show
  end

  functor WhitespaceTrivial (
    val whitespace : string list
    (* Amazingly there are utf8 sequences which
    * are whitespace outside of ascii set. What the hell. *)
  ) : TERMINAL =
    let
      val whitespace = List.map R.utf8 whitespace
    in
      RegexTerminal (
        type t = unit
        val regex =
          R.matching (fn i => List.exists (fn j => i = j) whitespace)
        fun map _ = ()
      )
    end

  (* "" surrounds, \ escapes the next character *)
  structure StringTerminal : REPL_TERMINAL = struct
    type t = string

    fun lex (ts : LS.stream) : (t * LS.stream) option =
      case LS.front ts of
        LS.Cons ( #"\"" , ts' ) =>
          let
            fun scan (acc , ts) =
              case LS.front ts of
                LS.Nil => NONE
              | LS.Cons ( #"\"" , ts' ) =>
                  SOME (String.implode (List.rev acc) , ts')
              | LS.Cons ( #"\\" , ts' ) =>
                  ( case LS.front ts' of
                      LS.Nil => NONE
                    | LS.Cons ( c , ts'' ) =>
                        scan (c :: #"\\" :: acc , ts'')
                  )
              | LS.Cons ( c , ts' ) =>
                  scan (c :: acc , ts')
          in
            scan (nil , ts')
          end
      | _ => NONE

    fun show t = "\"" ^ t ^ "\""

  end

end
