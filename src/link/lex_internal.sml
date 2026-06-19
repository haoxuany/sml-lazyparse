
signature STREAM = sig
  type 'a stream
  datatype 'a front = Nil | Cons of 'a * 'a stream
  val front : 'a stream -> 'a front
  val lazy : (unit -> 'a front) -> 'a stream
end

functor LexInternal (
  structure Stream : STREAM
) = struct

  structure TaggedStream = struct
    type stream = char Stream.stream * Annot.pos

    datatype front = Nil | Cons of char * stream

    fun front (( s , pos ) : stream ) : front * Annot.pos =
      case Stream.front s of
        Stream.Nil => ( Nil , pos )
      | Stream.Cons ( #"\r" , s' ) =>
          (* peek ahead for \r\n *)
          ( case Stream.front s' of
              Stream.Cons ( #"\n" , _ ) =>
                (* \r\n: \r stays on same line, \n will bump *)
                ( Cons ( #"\r" , ( s' , Annot.sameline 1 pos ) ) , pos )
            | _ =>
                (* lone \r is a newline *)
                ( Cons ( #"\r" , ( s' , Annot.newline 1 pos ) ) , pos )
          )
      | Stream.Cons ( #"\n" , s' ) =>
          ( Cons ( #"\n" , ( s' , Annot.newline 1 pos ) ) , pos )
      | Stream.Cons ( c , s' ) =>
          ( Cons ( c , ( s' , Annot.sameline 1 pos ) ) , pos )

    fun fromStream (s : char Stream.stream) (a : Annot.pos) : stream =
      ( s , a )
  end

  datatype ('a , 'b , 'c) token =
    TokenKeyword of 'a * Annot.span
  | TokenTrivial of 'b * Annot.span
  | TokenOther of 'c * Annot.span

  
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

  structure TS = TaggedStream

  fun lex (s : char Stream.stream)
    (start : Annot.pos)
    (keywords : (string * 'a) list)
    (lexTrivial :
      (char Stream.stream * Annot.pos) ->
      ('b * char Stream.stream * Annot.pos) option
    )
    (others :
      ((char Stream.stream * Annot.pos) ->
       ('c * Annot.span * char Stream.stream * Annot.pos) option
      ) list
    ) : ('a , 'b , 'c) token Stream.stream =
    let
      val ts = TS.fromStream s start
      val trie = Trie.build keywords

      (* walk the trie against the tagged stream, keeping the
       * longest match seen so far *)
      fun tryKeywords (ts : TS.stream)
        : ('a * TS.stream) option =
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
                case TS.front ts of
                  ( TS.Nil , _ ) => best
                | ( TS.Cons ( c , ts' ) , _ ) =>
                    case Trie.CharMap.find children c of
                      NONE => best
                    | SOME child => go (child , ts' , best)
            end
        in
          go (trie , ts , NONE)
        end

      (* try the trivial lexer *)
      fun tryTrivial (ts : TS.stream)
        : ('b * TS.stream) option =
        case lexTrivial ts of
          NONE => NONE
        | SOME (v , s , pos) => SOME (v , (s , pos))

      (* try the other terminal lexers, first match wins *)
      fun tryOthers (ts : TS.stream)
        : ('c * Annot.span * TS.stream) option =
        let
          fun go nil = NONE
            | go (lexer :: rest) =
                case lexer ts of
                  SOME (v , sp , s , pos) => SOME (v , sp , (s , pos))
                | NONE => go rest
        in
          go others
        end

      (* main lexing loop, produces a token stream *)
      fun go (ts : TS.stream) : ('a , 'b , 'c) token Stream.front =
        let val ( _ , pos ) = ts
        in
          case TS.front ts of
            ( TS.Nil , _ ) => Stream.Nil
          | ( TS.Cons ( c , rest ) , _ ) =>
              (* 1. try keywords *)
              case tryKeywords ts of
                SOME (v , ts) =>
                  let val ( _ , endpos ) = ts
                  in Stream.Cons ( TokenKeyword (v , Annot.span pos endpos)
                                 , Stream.lazy (fn () => go ts) )
                  end
              | NONE =>
              (* 2. try trivial *)
              case tryTrivial ts of
                SOME (v , ts) =>
                  let val ( _ , endpos ) = ts
                  in Stream.Cons ( TokenTrivial (v , Annot.span pos endpos)
                                 , Stream.lazy (fn () => go ts) )
                  end
              | NONE =>
              (* 3. try other terminals *)
              case tryOthers ts of
                SOME (v , sp , ts) =>
                  Stream.Cons ( TokenOther (v , sp)
                              , Stream.lazy (fn () => go ts) )
              | NONE =>
              (* 4. unrecognized character *)
              raise LexError (c , pos)
        end
    in
      Stream.lazy (fn () => go ts)
    end
end
