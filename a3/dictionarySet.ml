open Dictionary

module type ElementSig = sig
  type t

  include Dictionary.KeySig with type t := t
end

module type Set = sig
  module Elt : ElementSig

  type elt = Elt.t

  type t

  val rep_ok : t -> t

  val empty : t

  val is_empty : t -> bool

  val size : t -> int

  val insert : elt -> t -> t

  val member : elt -> t -> bool

  val remove : elt -> t -> t

  val union : t -> t -> t

  val intersect : t -> t -> t

  val difference : t -> t -> t

  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc

  val to_list : t -> elt list

  val format : Format.formatter -> t -> unit
end

module Make =
functor
  (E : ElementSig)
  (DM : DictionaryMaker)
  ->
  struct
    module Elt = E

    type elt = Elt.t

    (* TODO: change type [t] to something involving a dictionary *)

    (** AF: TODO: document the abstraction function. RI: TODO: document
        any representation invariants. *)
    type t = unit

    let rep_ok s = failwith "Unimplemented"

    let empty = ()
    (* TODO: replace [()] with a value of your rep type [t]. *)

    let is_empty s = failwith "Unimplemented"

    let size s = failwith "Unimplemented"

    let insert x s = failwith "Unimplemented"

    let member x s = failwith "Unimplemented"

    let remove x s = failwith "Unimplemented"

    let fold f init s = failwith "Unimplemented"

    let union s1 s2 = failwith "Unimplemented"

    let intersect s1 s2 = failwith "Unimplemented"

    let difference s1 s2 = failwith "Unimplemented"

    let to_list s = failwith "Unimplemented"

    let format fmt d = Format.fprintf fmt "<unimplemented>"
  end
