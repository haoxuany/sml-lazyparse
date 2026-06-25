
signature STREAM = sig
  type 'a stream
  datatype 'a front = Nil | Cons of 'a * 'a stream
  val front : 'a stream -> 'a front
  val lazy : (unit -> 'a front) -> 'a stream
end

functor TaggedStream (
  structure Stream : STREAM
  structure AnnotState : sig
    type token
    type t
    type 'a stream

    val next : t -> ( token * (token stream) ) -> t
    val pos : t -> Annot.pos
  end
    where type 'a stream = 'a Stream.stream
) = struct
  type stream = AnnotState.token Stream.stream * AnnotState.t

  datatype front = Nil | Cons of AnnotState.token * stream

  fun front ( (s , state) : stream ) : front =
    case Stream.front s of
      Stream.Nil => Nil
    | Stream.Cons ( v , tail ) =>
        Cons ( v , ( tail , AnnotState.next state ( v , tail ) ) )

  fun pos ( ( _ , state ) : stream) : Annot.pos =
    AnnotState.pos state

  fun fromStream s state = ( s , state )

end

functor LexInternal (
  structure Stream : STREAM
  structure Keyword : sig
    type t

    val keywords : (string * t) list
  end
  structure Trivial : TERMINAL
    where type 'a stream = 'a Stream.stream
  structure Terminal : sig
    type t
    type 'a stream

    val lex : ((char stream * Annot.pos) -> (t * char stream * Annot.pos)
    option) list
  end
    where type 'a stream = 'a Stream.stream
) = struct

  structure LexStream = TaggedStream (
    structure Stream = Stream
    structure AnnotState = struct
      type token = char
      type t = Annot.pos
      type 'a stream = 'a Stream.stream

      fun next pos (#"\r" , s) =
            ( case Stream.front s of
                Stream.Cons ( #"\n" , _ ) => Annot.sameline 1 pos
              | _ => Annot.newline 1 pos )
        | next pos (#"\n" , _) = Annot.newline 1 pos
        | next pos (_, _) = Annot.sameline 1 pos

      fun pos p = p
    end
  )

  structure LS = LexStream

  datatype token =
    TokenKeyword of Keyword.t * Annot.span
  | TokenTrivial of Trivial.t * Annot.span
  | TokenOther of Terminal.t * Annot.span

  structure TokenStream = TaggedStream (
    structure Stream = Stream
    structure AnnotState = struct
      type token = token
      type t = Annot.pos
      type 'a stream = 'a Stream.stream

      fun spanOf (TokenKeyword ( _ , sp )) = sp
        | spanOf (TokenTrivial ( _ , sp )) = sp
        | spanOf (TokenOther ( _ , sp )) = sp

      fun next _ ( tok , _ ) = #finish (spanOf tok)

      fun pos p = p
    end
  )


  exception LexError of char * Annot.pos

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
      fun tryTrivial (ts : LS.stream)
        : (Trivial.t * LS.stream) option =
        case lexTrivial ts of
          NONE => NONE
        | SOME (v , s , pos) => SOME (v , (s , pos))

      fun tryOthers (ts : LS.stream)
        : (Terminal.t * LS.stream) option =
        List.foldl
          (fn (lexer , best) =>
            case lexer ts of
              NONE => best
            | SOME (v , s , endpos) =>
                let val candidate = (v , (s , endpos))
                in
                  case best of
                    NONE => SOME candidate
                  | SOME ( _ , ( _ , bestPos )) =>
                      case Annot.compare (endpos , bestPos) of
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
                val keyword =
                  case tryKeywords ts of
                    SOME (v , ts') =>
                      let val endpos = LS.pos ts'
                      in SOME (TokenKeyword (v , Annot.span pos endpos) , endpos , ts')
                      end
                  | NONE => NONE

                val trivial =
                  case tryTrivial ts of
                    SOME (v , ts') =>
                      let val endpos = LS.pos ts'
                      in SOME (TokenTrivial (v , Annot.span pos endpos) , endpos , ts')
                      end
                  | NONE => NONE

                val other =
                  case tryOthers ts of
                    SOME (v , ts') =>
                      let val endpos = LS.pos ts'
                      in SOME (TokenOther (v , Annot.span pos endpos) , endpos , ts')
                      end
                  | NONE => NONE

                val candidates =
                  List.mapPartial (fn x => x) [keyword , trivial , other]
              in
                case candidates of
                  nil => raise LexError (c , pos)
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
