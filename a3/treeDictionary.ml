open Dictionary

module Make =
functor
  (K : KeySig)
  (V : ValueSig)
  ->
  struct
    module Key = K
    module Value = V

    type key = K.t

    type value = V.t

    (* TODO: change type [t] from [unit] to something involving
       red-black trees. *)
    (* AF: TODO: document the abstraction function.
     * RI: TODO: document any representation invariants. *)
    type t = unit

    let rep_ok d = failwith "Unimplemented"

    let empty = ()
    (* TODO: replace [()] with a value of your rep type [t]. *)

    let is_empty d = failwith "Unimplemented"

    let size d = failwith "Unimplemented"

    let insert k v d = failwith "Unimplemented"

    let remove k d = failwith "optional"

    let find k d = failwith "Unimplemented"

    let member k d = failwith "Unimplemented"

    let to_list d = failwith "Unimplemented"

    let fold f acc d = failwith "Unimplemented"

    let format fmt d = Format.fprintf fmt "<unimplemented>"
  end
