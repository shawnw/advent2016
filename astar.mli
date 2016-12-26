module type LocationType =
  sig
    type t
    val guess_distance : t -> t -> int
    val neighbors : t -> (t * int) list
    val equal : t -> t -> bool
    val hash : t -> int
  end
module Make :
  functor (Location : LocationType) ->
    sig val distance : Location.t -> Location.t -> int end
