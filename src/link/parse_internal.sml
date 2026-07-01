
(* common internal code to avoid codegening everything *)
functor ParseInternal (
  structure Trivial : TERMINAL
  structure Terminal : sig
    type t

    (* for error reporting *)
    val name : t -> string

    val lex : (LexStream.stream -> (t * LexStream.stream) option) list
  end
  val keywords : (string * int) list
) = struct

  type 'a annot = { node : 'a , span : Annot.span }

  fun impossible () = raise Fail "Impossible"

  structure LexInternal = LexInternal (
    structure Keyword = struct
      type t = int
      val keywords = keywords
    end
    structure Trivial = Trivial
    structure Terminal = Terminal
  )

  datatype internal_error =
    InternalErrorUnexpectedEOF
  | InternalErrorUnknown (* internal *)
  | InternalErrorExpectedKeyword of int
  | InternalErrorExpectedTerminal of string

  type internal_error = 
    { sort : string option
    , rule : string option
    , error : internal_error
    }

  fun newerror error : internal_error =
    { sort = NONE
    , rule = NONE
    , error = error
    }

  structure Parcom = ParcomError (
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
    structure ParseError = struct
      type t = internal_error
      val unexpected_eof =
        newerror InternalErrorUnexpectedEOF
    end
  )

  open Parcom

  val skipTrivial = starLongest (terminal (fn
    LexInternal.TokenTrivial _ => ParseSuccess ()
  | _ => ParseFailure (newerror InternalErrorUnknown)))

  fun keyword k =
    bind skipTrivial (fn _ =>
      terminal (fn
        LexInternal.TokenKeyword (k' , sp) =>
          if k = k'
          then ParseSuccess { node = () , span = sp }
          else ParseFailure
            (newerror (InternalErrorExpectedKeyword k))
      | _ => ParseFailure
        (newerror (InternalErrorExpectedKeyword k)))
    )

  fun parseTerminal proj name =
    bind skipTrivial (fn _ =>
      terminal (fn
        LexInternal.TokenOther (v , sp) =>
          (case proj v of
            SOME t =>
              ParseSuccess { node = { node = t , span = sp } , span = sp }
          | NONE => ParseFailure 
            (newerror (InternalErrorExpectedTerminal name))
          )
      | _ => ParseFailure
        (newerror (InternalErrorExpectedTerminal name)))
    )

  (* In the case of empty parse, recover span from stream position *)
  fun empty ( node : 'a ) : 'a annot Parcom.t =
    terminals (fn s =>
    let
      val pos = LexInternal.TokenStream.pos s
    in
      ParseSuccess
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
      nil => impossible ()
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

  fun longest (v : 'a t)
    : 'a t =
    Parcom.join
      (List.foldr
        (fn ( candidate as ( _ , ( s , _ ) ) , best ) =>
          case best of
            nil => [candidate]
          | [( _ , ( bs , _ ) )] =>
              let
                val cpos = LexInternal.TokenStream.pos s
                val bpos = LexInternal.TokenStream.pos bs
              in
                case Annot.compare ( cpos , bpos ) of
                  GREATER => [candidate]
                | EQUAL => candidate :: best
                | _ => best
              end
          | _ => best)
        nil)
       v

  type 'a parser = 'a t_memo
  type token_stream =
    Parcom.stream

  fun lex s pos =
    LexInternal.TokenStream.fromStream (LexInternal.lex s pos) pos

  exception LexError = LexInternal.LexError

  fun return_node (node : 'a) (l : Annot.span list)
    : 'a annot Parcom.t  =
    return { node = node , span = annot_list l }

  fun annot_add ({ span , ... } : 'a annot)
    : Annot.span = span

  fun create rule_construct sort' rule' node = 
    mapError 
    (fn ({ error , sort , rule } : internal_error) =>
      { sort = case sort of NONE => SOME sort' | SOME _ => sort
      , rule = case rule of NONE => SOME rule' | SOME _ => rule
      , error = error })
    (map rule_construct node)

  datatype 'a result =
    Success of ('a * token_stream) list
  | Fail of ParseError.t

  structure TS = LexInternal.TokenStream

  structure IntDict = SplayDict (structure Key = IntOrdered)

  val keywordDict =
    List.foldr (fn ( (s , i) , d ) =>
      IntDict.insert d i s 
    ) IntDict.empty keywords

  fun parse p stream =
    case parser p stream of
      ResultSuccess v => Success v
    | ResultFailure errors =>
        let
          val ({ sort , rule , error } , stream) = List.hd errors
          (* In generated code, these really should just exist due
          * to the map. *)
          val sort = case sort of SOME v => v | NONE => "(unknown)"
          val rulename =
            case rule of SOME v => v | NONE => "(unknown)"

          fun next s =
            case TS.front s of
              TS.Nil => ( NONE , TS.pos s )
            | TS.Cons ( v , s ) =>
                (case v of
                  LexInternal.TokenTrivial _ => next s
                | LexInternal.TokenKeyword ( i , span ) =>
                    ( SOME ( IntDict.lookup keywordDict i ) 
                    , #start span 
                    )
                | LexInternal.TokenOther ( term , span ) =>
                    ( SOME ( Terminal.name term ) 
                    , #start span 
                    )
                )

          val ( actual , pos ) = next stream
        in
          Fail (
          case error of
            InternalErrorUnexpectedEOF =>
              ParseError.UnexpectedEOF
              { sort = sort
              , rulename = rulename
              , pos = pos
              }
          | InternalErrorUnknown => impossible ()
          | InternalErrorExpectedKeyword k =>
              ParseError.ExpectedKeyword
              { sort = sort
              , rulename = rulename
              , keyword = IntDict.lookup keywordDict k
              , actual = case actual of SOME s => s | NONE => "(eof)"
              , pos = pos
              }
          | InternalErrorExpectedTerminal name =>
              ParseError.ExpectedTerminal
              { sort = sort
              , rulename = rulename
              , terminal = name
              , actual = case actual of SOME s => s | NONE => "(eof)"
              , pos = pos
              }
          )
        end

  (* Have some way of working with token streams externally *)
  structure TokenStream = struct
    type t = token_stream
    datatype token =
      Keyword of string * Annot.span
    | Terminal of Terminal.t * Annot.span

    datatype front = Nil | Cons of token * t

    fun front (s : token_stream) : front =
      case TS.front s of
        TS.Nil => Nil
      | TS.Cons ( v , s ) =>
          (case v of
            LexInternal.TokenTrivial _ => front s
          | LexInternal.TokenKeyword ( i , span ) =>
              Cons ( Keyword ( IntDict.lookup keywordDict i , span ) , s )
          | LexInternal.TokenOther ( term , span ) =>
              Cons ( Terminal ( term , span ) , s )
          )

    fun pos (s : token_stream) : Annot.pos = 
      LexInternal.TokenStream.pos s
  end

end
