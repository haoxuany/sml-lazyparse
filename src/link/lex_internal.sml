
functor LexInternal (
  structure Keyword : sig
    type t

    val keywords : (string * t) list
  end
  structure Trivial : TERMINAL
  structure Terminal : sig
    type t

    val lex : (LexStream.stream -> (t * LexStream.stream) option) list
  end
) = struct

  structure LS = LexStream

  datatype token =
    TokenKeyword of Keyword.t * Annot.span
  | TokenTrivial of Trivial.t * Annot.span
  | TokenOther of Terminal.t * Annot.span

  structure TokenStream = TaggedStream (
    structure AnnotState = struct
      type token = token
      type t = Annot.pos

      fun next _ ( tok , _ ) =
        let
          val { finish , ... } =
            case tok of
              TokenKeyword ( _ , sp ) => sp
            | TokenTrivial ( _ , sp ) => sp
            | TokenOther ( _ , sp ) => sp
        in finish end

      fun pos p = p
    end
  )


  exception LexError of LexStream.stream

  structure Trie = struct
    structure CharMap = SplayDict (structure Key = CharOrdered)

    datatype 'a t = Trie of 'a option * 'a t CharMap.dict

    val empty = Trie (NONE , CharMap.empty)

    fun insert (Trie (v , children)) (s : string) (x : 'a) : 'a t =
      let
        fun go cs (Trie (v , children)) =
          case cs of
            nil => Trie (SOME x , children)
          | c :: cs' =>
              let
                val child = case CharMap.find children c of
                              SOME t => t
                            | NONE => empty
              in
                Trie (v , CharMap.insert children c (go cs' child))
              end
      in
        go (String.explode s) (Trie (v , children))
      end

    fun build (keywords : (string * 'a) list) : 'a t =
      List.foldl (fn ((s , x) , t) => insert t s x) empty keywords
  end

  fun lex (s : char Stream.stream)
    (start : Annot.pos)
    : token Stream.stream =
    let
      val keywords = Keyword.keywords
      val lexTrivial = Trivial.lex
      val others = Terminal.lex

      val ts = LS.fromStream s start
      val trie = Trie.build keywords

      (* walk the trie against the tagged stream, keeping the
       * longest match seen so far *)
      fun tryKeywords (ts : LS.stream)
        : (Keyword.t * LS.stream) option =
        let
          fun go (Trie.Trie (v , children) , ts , best) =
            let
              val best =
                case v of
                  SOME x => SOME (x , ts)
                | NONE => best
            in
              if Trie.CharMap.isEmpty children then best
              else
                case LS.front ts of
                  LS.Nil => best
                | LS.Cons ( c , ts' ) =>
                    case Trie.CharMap.find children c of
                      NONE => best
                    | SOME child => go (child , ts' , best)
            end
        in
          go (trie , ts , NONE)
        end

      (* try the trivial lexer *)

      fun tryOthers (ts : LS.stream)
        : (Terminal.t * LS.stream) option =
        List.foldl
          (fn (lexer , best) =>
            case lexer ts of
              NONE => best
            | SOME (v , ts') =>
                let val candidate = (v , ts')
                    val endpos = LS.pos ts'
                in
                  case best of
                    NONE => SOME candidate
                  | SOME ( _ , bestTs ) =>
                      case Annot.compare (endpos , LS.pos bestTs) of
                        GREATER => SOME candidate
                      | _ => best
                end)
          NONE others

      (* main lexing loop, produces a token stream *)
      fun go (ts : LS.stream) : token Stream.front =
        let val pos = LS.pos ts
        in
          case LS.front ts of
            LS.Nil => Stream.Nil
          | LS.Cons ( c , _ ) =>
              let
                fun wrap tokCon result =
                  case result of
                    NONE => NONE
                  | SOME (v , ts') =>
                      let val endpos = LS.pos ts'
                      in SOME (tokCon (v , Annot.span pos endpos) , endpos , ts')
                      end

                val keyword = wrap TokenKeyword (tryKeywords ts)
                val trivial = wrap TokenTrivial (lexTrivial ts)
                val other = wrap TokenOther (tryOthers ts)

                val candidates =
                  List.mapPartial (fn x => x) [keyword , trivial , other]
              in
                case candidates of
                  nil => raise LexError ts
                | first :: rest =>
                    let
                      val (tok , _ , ts') =
                        List.foldl
                          (fn (candidate as ( _ , candidatePos , _ ) , acc as ( _ , accPos , _ )) =>
                            case Annot.compare (candidatePos , accPos) of
                              GREATER => candidate
                            | _ => acc)
                          first rest
                    in
                      Stream.Cons (tok , Stream.lazy (fn () => go ts'))
                    end
              end
        end
    in
      Stream.lazy (fn () => go ts)
    end
end
