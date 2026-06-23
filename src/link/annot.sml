
structure Annot = struct
  
  type pos =
    { pos : int (* byte position *)
    , lineno : int (* 1-indexed *)
    , colno : int (* 1-indexed *)
    }

  type span =
    { start : pos
    , finish : pos
    }

  val empty = { pos = 0 , lineno = 1 , colno = 1 }
  
  fun newline byteshift { pos , lineno , colno } = (* for \n, \r, \r\n *)
    { pos = pos + byteshift
    , lineno = lineno + 1
    , colno = 1
    }
  
  fun sameline byteshift { pos , lineno , colno } = (* same line strings *)
    { pos = pos + byteshift
    , lineno = lineno
    , colno = colno + byteshift
    }

  fun span (a : pos) (b : pos) =
    { start = a
    , finish = b
    }

  fun join ({ start , ... } : span) ({ finish , ... } : span) : span =
    { start = start
    , finish = finish
    }

  fun length
    ({ start = { pos = a_pos , ... } : pos , finish = { pos = b_pos , ... } : pos } : span)
    = b_pos - a_pos

  fun compare
    ( { pos = a_pos , ... } : pos
    , { pos = b_pos , ... } : pos
    ) =
    Int.compare (a_pos , b_pos)
end
