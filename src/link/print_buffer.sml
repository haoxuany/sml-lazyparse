
structure PrintBuffer = struct

  (* lines are reversed, strings within each line are reversed *)
  type t = (string list list * int) ref

  fun empty () : t = ref ( [ nil ] , 1 )

  fun push
    ( buffer : t )
    ( v : string )
    ( targetLine : int ) : unit =
    let
      val ( lines , lineno ) = !buffer
      val cur = case lines of l :: _ => l | nil => nil
      val rest = case lines of _ :: r => r | nil => nil
    in
      if targetLine > lineno
      then
        buffer := ( [v] :: cur :: rest , targetLine )
      else
        buffer := ( (v :: " " :: cur) :: rest , lineno )
    end

  fun toString ( buffer : t ) : string =
    let
      val ( lines , _ ) = !buffer
      val lines = List.rev (List.map (fn l => String.concat (List.rev l)) lines)
    in
      String.concatWith "\n" lines
    end
end
