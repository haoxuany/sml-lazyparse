
(* common internal code to avoid codegening everything *)
functor ParseInternal (
  structure Stream : STREAM
  structure Trivial : TERMINAL
    where type 'a stream = 'a Stream.stream
  structure Terminal : sig
    type t
    type 'a stream

    val lex : ((char stream * Annot.pos) -> (t * char stream * Annot.pos)
    option) list
  end
    where type 'a stream = 'a Stream.stream
  val keywords : (string * int) list
) = struct

  type 'a annot = { node : 'a , span : Annot.span }

  structure LexInternal = LexInternal (
    structure Stream = Stream
    structure Keyword = struct
      type t = int
      val keywords = keywords
    end
    structure Trivial = Trivial
    structure Terminal = Terminal
  )

  structure Parcom = Parcom (
    structure TokenStream = struct
      structure TS = LexInternal.TokenStream
      type token = LexInternal.token
      type stream = TS.stream

      datatype front = Nil | Cons of token * stream
      val front : stream -> front =
        fn s => case TS.front s of
          TS.Nil => Nil
        | TS.Cons (h, t) => Cons (h, t)
    end
  )

  open Parcom

  val skipTrivial = starLongest (remove (fn
    LexInternal.TokenTrivial _ => true
  | _ => false))

  fun keyword k =
    bind skipTrivial (fn _ =>
      terminal (fn
        LexInternal.TokenKeyword (k' , sp) =>
          if k = k'
          then SOME { node = () , span = sp }
          else NONE
      | _ => NONE)
    )

  fun parseTerminal proj =
    bind skipTrivial (fn _ =>
      terminal (fn
        LexInternal.TokenOther (v , sp) =>
          (case proj v of
            SOME t =>
              SOME { node = { node = t , span = sp } , span = sp }
          | NONE => NONE)
      | _ => NONE)
    )

  (* In the case of empty parse, recover span from stream position *)
  fun empty ( node : 'a ) : 'a annot Parcom.t =
    terminals (fn s =>
    let
      val pos = LexInternal.TokenStream.pos s
    in
      SOME
      ( { node = node , span = Annot.span pos pos } , 0 , s )
    end
    )

  fun map f (p : 'a annot Parcom.t) : 'b annot Parcom.t =
    Parcom.map 
    (fn { node , span } =>
      { node = f node , span = span }) p
    

  fun parseNonterminal nonterminal =
    bind nonterminal (fn ( v as { span , ... } : 'a annot ) =>
      return { node = v , span = span })

  fun annot_list (l : Annot.span list) : Annot.span =
    case l of
      nil => raise Fail "Impossible"
    | first :: rest =>
        List.foldl
        (fn ( new , { start , finish } ) =>
          let
            val { start = nstart , finish = nfinish } = new

            val start =
              case Annot.compare ( nstart , start ) of
                LESS => nstart
              | EQUAL => nstart
              | GREATER => start

            val finish =
              case Annot.compare ( finish , nfinish ) of
                LESS => nfinish
              | EQUAL => nfinish
              | GREATER => finish
          in
            { start = start , finish = finish }
          end
        )
        first rest

  fun optionalLongest (v : 'a annot t)
    : 'a option annot t =
      Parcom.prefer [ map SOME v , empty NONE ] 

  fun starLongest (v : 'a annot t)
    : 'a list annot t =
    bind (Parcom.starLongest v) (fn v =>
      case v of
        nil => empty nil
      | _ => return
          { node = List.map #node v
          , span = annot_list (List.map #span v)
          })

  fun plusLongest (v : 'a annot t)
    : 'a list annot t =
    bind (Parcom.plusLongest v) (fn v =>
      return
        { node = List.map #node v
        , span = annot_list (List.map #span v)
        })


  type 'a parser = 'a t_memo
  type token_stream =
    Parcom.stream

  fun lex s pos =
    LexInternal.TokenStream.fromStream (LexInternal.lex s pos) pos

  fun return_node (node : 'a) (l : Annot.span list)
    : 'a annot Parcom.t  =
    return { node = node , span = annot_list l }

  fun annot_add ({ span , ... } : 'a annot)
    : Annot.span = span

  fun create (f : 'a -> 'b) (p : 'a annot Parcom.t)
    : 'b annot Parcom.t =
    Parcom.map
    (fn { node , span } => { node = f node , span = span })
    p

end
