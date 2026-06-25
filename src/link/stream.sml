
functor TaggedStream (
  structure AnnotState : sig
    type token
    type t

    val next : t -> ( token * ( token Stream.stream ) ) -> t
    val pos : t -> Annot.pos
  end
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

structure LexStream : sig
  type stream

  datatype front = Nil | Cons of char * stream

  val front : stream -> front
  val pos : stream -> Annot.pos

  val fromStream : char Stream.stream -> Annot.pos -> stream

end = TaggedStream (
  structure AnnotState = struct
    type token = char
    type t = Annot.pos

    fun next pos (#"\r" , s) =
          ( case Stream.front s of
              Stream.Cons ( #"\n" , _ ) => Annot.sameline 1 pos
            | _ => Annot.newline 1 pos )
      | next pos (#"\n" , _) = Annot.newline 1 pos
      | next pos (_, _) = Annot.sameline 1 pos

    fun pos p = p
  end
)

