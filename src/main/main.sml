structure Main = struct

  structure Parser = ParseParseFn(Lex)

  fun usage () =
    ( print "usage: lazyparse <file.ungram> [-o <name.sml>]\n"
    ; OS.Process.exit OS.Process.failure
    )

  fun die msg =
    ( print (String.concat ["error: " , msg , "\n"])
    ; OS.Process.exit OS.Process.failure
    )

  fun stripExtension filename =
    case OS.Path.splitBaseExt filename of
      { base , ext = _ } => OS.Path.file base

  fun stripSmlExtension filename =
    case OS.Path.splitBaseExt filename of
      { base , ext = SOME "sml" } => base
    | _ => filename

  fun reportRepairs sm repairs =
    List.app
      (fn repair =>
        print (String.concat
          [AntlrRepair.repairToString ParseTokens.toString sm repair , "\n"]))
      repairs

  fun run (filename , output) =
    let
      val name = stripSmlExtension output
      val ins = TextIO.openIn filename
        handle IO.Io { cause , ... } =>
          die (String.concat ["cannot open file '" , filename , "': " , exnMessage cause])
      val sm = AntlrStreamPos.mkSourcemap' filename
      val strm = Lex.streamifyInstream ins
      val (result , _ , repairs) = Parser.parse (Lex.lex sm) strm
      val _ = TextIO.closeIn ins
      val _ =
        case repairs of
          nil => ()
        | _ => reportRepairs sm repairs
    in
      case result of
        SOME grammar =>
          ( Codegen.codegen name (Elaboration.elaborate grammar)
            handle Elaboration.UnboundNonterminal s =>
              die (String.concat ["unbound nonterminal '" , s , "'"])
            | Elaboration.MissingRuleName =>
              die "alternative is missing a [ name ... ] annotation"
            | Elaboration.DuplicateRuleName =>
              die "alternative has duplicate [ name ... ] annotations"
          ; print (String.concat ["wrote " , name , ".sml\n"])
          )
      | NONE =>
          die (String.concat ["parse error in '" , filename , "'"])
    end

  fun main () =
    case CommandLine.arguments () of
      [filename] => run (filename , stripExtension filename)
    | [filename , "-o" , output] => run (filename , output)
    | _ => usage ()

end
