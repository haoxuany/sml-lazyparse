
structure Regex 
:> sig
  type t

  val epsilon : t

  val exact : char -> t
  val exactOrd : int -> t
  val exactUtf8 : string -> t
  (* int represents utf8 codepoints *)
  val matching : (int -> bool) -> t
  val any : t
  val except : (int -> bool) -> t

  val charRange : char * char -> t
  val ordRange : int * int -> t
  val utf8Range : string * string -> t

  val alt : t list -> t
  val seq : t list -> t
  val star : t -> t
  val plus : t -> t
  val opt : t -> t

  val string : string -> t
  val set : char list -> t

  val digit : t
  val lower : t
  val upper : t
  val alpha : t
  val alphaNum : t
  val whitespace : t

  val utf8 : string -> int

  val regex : t -> LexStream.stream -> (string * LexStream.stream) option

end = struct
  datatype t =
    Matching of (int -> bool)
  | Alt of t list
  | Seq of t list
  | Star of t

  val epsilon = Seq nil

  fun exact c = Matching (fn i => i = Char.ord c)
  fun exactOrd i = Matching (fn j => j = i)

  fun charRange (lo , hi) =
    let val lo = Char.ord lo val hi = Char.ord hi
    in Matching (fn i => lo <= i andalso i <= hi)
    end
  fun ordRange (lo , hi) = Matching (fn i => lo <= i andalso i <= hi)

  fun utf8 (s : string) : int =
    let
      val bytes = List.map Char.ord (String.explode s)
    in
      case bytes of
        [b] => b
      | [b0 , b1] =>
          (b0 - 0xC0) * 64
          + (b1 - 0x80)
      | [b0 , b1 , b2] =>
          (b0 - 0xE0) * 4096
          + (b1 - 0x80) * 64
          + (b2 - 0x80)
      | [b0 , b1 , b2 , b3] =>
          (b0 - 0xF0) * 262144
          + (b1 - 0x80) * 4096
          + (b2 - 0x80) * 64
          + (b3 - 0x80)
      | _ => raise Fail "Regex.utf8Codepoint: invalid UTF-8"
    end

  fun exactUtf8 s = exactOrd (utf8 s)
  fun utf8Range (lo , hi) = ordRange (utf8 lo , utf8 hi)

  fun matching f = Matching f

  val any = Matching (fn _ => true)

  fun except f = Matching (fn i => not (f i))

  fun alt l =
    case List.concatMap (fn Alt l => l | r => [r]) l of
      [r] => r
    | l => Alt l

  fun seq l =
    let
      val l = List.concatMap (fn Seq l => l | r => [r]) l
    in
      case l of
        [r] => r
      | l => Seq l
    end

  fun star r =
    case r of
      Seq nil => Seq nil
    | Star _ => r
    | _ => Star r

  fun plus r =
    case r of
      Seq nil => Seq nil
    | _ => seq [r , star r]

  fun opt r =
    case r of
      Seq nil => Seq nil
    | _ => alt [r , Seq nil]

  fun string s =
    seq (List.map exact (String.explode s))

  fun set cs =
    let val ords = List.map Char.ord cs
    in Matching (fn i => List.exists (fn j => i = j) ords)
    end

  val digit = charRange (#"0" , #"9")
  val lower = charRange (#"a" , #"z")
  val upper = charRange (#"A" , #"Z")
  val alpha = alt [lower , upper]
  val alphaNum = alt [alpha , digit]
  val whitespace = set [#" " , #"\t" , #"\n" , #"\r"]

  structure LS = LexStream

  fun regex (r : t) (ts : LS.stream) : (string * LS.stream) option =
    let
      (* decode one utf8 codepoint from the lex stream, returning
       * the codepoint, the raw bytes as a string, and the remaining stream *)
      fun readCodepoint (ts : LS.stream) : (int * string * LS.stream) option =
        case LS.front ts of
          LS.Nil => NONE
        | LS.Cons ( c , ts' ) =>
            let val b = Char.ord c
            in
              if b < 0x80 then SOME (b , String.str c , ts')
              else if b < 0xC0 then NONE
              else if b < 0xE0 then
                ( case LS.front ts' of
                    LS.Cons ( c1 , ts'' ) =>
                      SOME ((b - 0xC0) * 64
                            + (Char.ord c1 - 0x80)
                           , String.implode [c , c1] , ts'')
                  | _ => NONE )
              else if b < 0xF0 then
                ( case LS.front ts' of
                    LS.Cons ( c1 , ts1 ) =>
                      ( case LS.front ts1 of
                          LS.Cons ( c2 , ts'' ) =>
                            SOME ((b - 0xE0) * 4096
                                  + (Char.ord c1 - 0x80) * 64
                                  + (Char.ord c2 - 0x80)
                                 , String.implode [c , c1 , c2] , ts'')
                        | _ => NONE )
                  | _ => NONE )
              else
                ( case LS.front ts' of
                    LS.Cons ( c1 , ts1 ) =>
                      ( case LS.front ts1 of
                          LS.Cons ( c2 , ts2 ) =>
                            ( case LS.front ts2 of
                                LS.Cons ( c3 , ts'' ) =>
                                  SOME ((b - 0xF0) * 262144
                                        + (Char.ord c1 - 0x80) * 4096
                                        + (Char.ord c2 - 0x80) * 64
                                        + (Char.ord c3 - 0x80)
                                       , String.implode [c , c1 , c2 , c3] , ts'')
                              | _ => NONE )
                        | _ => NONE )
                  | _ => NONE )
            end

      fun match r ts acc =
        case r of
          Matching f =>
            ( case readCodepoint ts of
                NONE => NONE
              | SOME (cp , s , ts') =>
                  if f cp then SOME (s :: acc , ts')
                  else NONE )
        | Seq nil => SOME (acc , ts)
        | Seq (first :: rest) =>
            ( case match first ts acc of
                NONE => NONE
              | SOME (acc' , ts') => match (Seq rest) ts' acc' )
        | Alt alts =>
            List.foldl
              (fn (alt , best) =>
                case match alt ts acc of
                  NONE => best
                | SOME (acc' , ts') =>
                    case best of
                      NONE => SOME (acc' , ts')
                    | SOME ( _ , bestTs ) =>
                        case Annot.compare (LS.pos ts' , LS.pos bestTs) of
                          GREATER => SOME (acc' , ts')
                        | _ => best)
              NONE alts
        | Star inner =>
            let
              fun search ts acc =
                case match inner ts acc of
                  NONE => (acc , ts)
                | SOME (acc' , ts') => search ts' acc'
            in
              SOME (search ts acc)
            end
    in
      case match r ts nil of
        NONE => NONE
      | SOME (acc , ts') =>
          SOME (String.concat (List.rev acc) , ts')
    end

end
