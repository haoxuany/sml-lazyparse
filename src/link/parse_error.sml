
structure ParseError : sig 
  datatype t =
    UnexpectedEOF of 
      { sort : string 
      , rulename : string 
      , pos : Annot.pos 
      }
  | ExpectedKeyword of
      { sort : string 
      , rulename : string 
      , keyword : string
      , actual : string (* terminal *)
      , pos : Annot.pos 
      }
  | ExpectedTerminal of
      { sort : string 
      , rulename : string 
      , terminal : string
      , actual : string (* keyword *)
      , pos : Annot.pos 
      }

  val show : t -> string
end = struct
  datatype t =
    UnexpectedEOF of 
      { sort : string 
      , rulename : string 
      , pos : Annot.pos 
      }
  | ExpectedKeyword of
      { sort : string 
      , rulename : string 
      , keyword : string
      , actual : string (* terminal *)
      , pos : Annot.pos 
      }
  | ExpectedTerminal of
      { sort : string 
      , rulename : string 
      , terminal : string
      , actual : string (* keyword *)
      , pos : Annot.pos 
      }

  fun show error =
    case error of
      UnexpectedEOF { sort , rulename , pos } =>
        let val { lineno , colno , ... } = pos
        in String.concat
          [ sort , ": " , rulename , ": "
          , "unexpected end of input at "
          , Int.toString lineno , ":" , Int.toString colno
          ]
        end
    | ExpectedKeyword { sort , rulename , keyword , actual , pos } =>
        let val { lineno , colno , ... } = pos
        in String.concat
          [ sort , ": " , rulename , ": "
          , "expected keyword '" , keyword , "'"
          , " but got '" , actual , "'"
          , " at " , Int.toString lineno , ":" , Int.toString colno
          ]
        end
    | ExpectedTerminal { sort , rulename , terminal , actual , pos } =>
        let val { lineno , colno , ... } = pos
        in String.concat
          [ sort , ": " , rulename , ": "
          , "expected " , terminal
          , " but got '" , actual , "'"
          , " at " , Int.toString lineno , ":" , Int.toString colno
          ]
        end

end
