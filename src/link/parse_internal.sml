
(* common internal code to avoid codegening everything *)
functor ParseInternal (
  val table_size : int
  structure Stream : STREAM
  structure Trivial : TERMINAL 
    where type 'a stream = 'a Stream.stream
  type terminal 
  val keywords : (string * int) list
) = struct

  type 'a annot = { node : 'a , span : Annot.span }

  structure LexInternal = LexInternal (
    structure Stream = Stream
  )

  structure Parcom = Parcom (
    type token = (int , Trivial.t , terminal) LexInternal.token
    val table_size = table_size
    structure Stream = Stream
  )

  open Parcom

  val skipTrivial = starLongest (remove (fn
    LexInternal.TokenTrivial _ => true
  | _ => false))

  type 'a node_inner =
    { node : 'a , span : Annot.span option }

  fun keyword k = 
    bind skipTrivial (fn _ => 
      terminal (fn
        LexInternal.TokenKeyword (k' , sp) => 
          if k = k' 
          then SOME { node = () , span = SOME sp } 
          else NONE
      | _ => NONE)
    )

  fun parseTerminal proj =
    bind skipTrivial (fn _ =>
      terminal (fn
        LexInternal.TokenOther (v , sp) =>
          (case proj v of
            SOME t =>
              SOME { node = { node = t , span = sp } , span = SOME sp }
          | NONE => NONE)
      | _ => NONE)
    )

  fun parseNonterminal nonterminal =
    bind nonterminal (fn ( v : 'a annot ) =>
      return { node = v , span = SOME (#span v) })

  fun optionalLongest (v : 'a node_inner t) 
    : 'a option node_inner t =
      bind (Parcom.optionalLongest v) (fn v =>
        return 
          (case v of
            NONE => { node = NONE , span = NONE }
          | SOME { node , span } => 
              { node = SOME node , span = span }
          ))

  fun annot_list (l : Annot.span option list) : Annot.span option =
    List.foldl 
    (fn ( new , current ) =>
      case new of 
        NONE => current
      | SOME new =>
          ( case current of
              NONE => SOME new
            | SOME { start , finish } =>
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
                  SOME { start = start , finish = finish }
                end
          )
    )
    NONE l

  fun starLongest (v : 'a node_inner t)
    : 'a list node_inner t =
    bind (Parcom.starLongest v) (fn v =>
      return
        { node = List.map #node v
        , span = annot_list (List.map #span v)
        })

  fun plusLongest (v : 'a node_inner t)
    : 'a list node_inner t =
    bind (Parcom.plusLongest v) (fn v =>
      return
        { node = List.map #node v
        , span = annot_list (List.map #span v)
        })


  type 'a parser = 'a t_memo
  type token_stream = 
    (int , Trivial.t , terminal) 
    LexInternal.token Stream.stream

  fun lex lexers s pos =
    LexInternal.lex s pos keywords Trivial.lex lexers

  fun addLexer lex inj =
    fn x => 
      ( case lex x of 
        SOME ( v , s , p ) => SOME ( inj v , s , p )
      | NONE => NONE
      )

  fun return_node (node : 'a) (l : Annot.span option list) 
    : 'a node_inner Parcom.t  =
    return { node = node , span = annot_list l }
      
  fun annot_add ({ span , ... } : 'a node_inner) 
    : Annot.span option = span

  fun create (f : 'a -> 'b) (p : 'a node_inner Parcom.t) 
    : 'b annot Parcom.t =
    Parcom.map 
    (fn { node , span } =>
    let
      val span =
        case span of
          NONE => Annot.span Annot.empty Annot.empty
        | SOME span => span
    in
      { node = f node , span = span }
    end)
    p

end
