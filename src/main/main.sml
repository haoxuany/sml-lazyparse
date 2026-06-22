structure Main = struct

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

  fun run (filename , output) =
    let
      val name = stripSmlExtension output
      val ins = TextIO.openIn filename
        handle IO.Io { cause , ... } =>
          die (String.concat ["cannot open file '" , filename , "': " , exnMessage cause])
      val sm = Parse.mkSourcemap' filename
      val strm = Parse.streamifyInstream ins
      val result = Parse.parse sm strm
        handle Parse.LexError msg => die msg
      val _ = TextIO.closeIn ins
    in
      Codegen.codegen name (Elaboration.elaborate result)
        handle Elaboration.UnboundNonterminal s =>
          die (String.concat ["unbound nonterminal '" , s , "'"])
        | Elaboration.MissingRuleName =>
          die "alternative is missing a [ name ... ] annotation"
        | Elaboration.DuplicateProperty s =>
          die (String.concat ["duplicate property '" , s , "'"])
        | Elaboration.InvalidAssociativity s =>
          die s
      ; print (String.concat ["wrote " , name , ".sml\n"])
    end

  fun main () =
    case CommandLine.arguments () of
      [filename] => run (filename , stripExtension filename)
    | [filename , "-o" , output] => run (filename , output)
    | _ => usage ()

end
