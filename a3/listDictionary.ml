open Dictionary

(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association
    list [lst] as a dictionary. The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively. The [fmt]
    argument is where to put the formatted output. *)
let format_assoc_list format_key format_val fmt lst =
  (* You are free to improve the output of this function in any way you
     like. *)
  Format.fprintf fmt "[";
  List.iter
    (fun (k, v) ->
      Format.fprintf fmt "%a -> %a; " format_key k format_val v)
    lst;
  Format.fprintf fmt "]"
  [@@coverage off]

module Make : DictionaryMaker =
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
       association lists. *)

    (** AF: TODO: document the abstraction function. RI: TODO: document
        any representation invariants. *)
    type t = unit

    let rep_ok d = failwith "Unimplemented"

    let empty = ()
    (* TODO: replace [()] with a value of your rep type [t]. *)

    let is_empty d = failwith "Unimplemented"

    let size d = failwith "Unimplemented"

    let insert k v d = failwith "Unimplemented"

    let remove k d = failwith "Unimplemented"

    let find k d = failwith "Unimplemented"

    let member k d = failwith "Unimplemented"

    let to_list d = failwith "Unimplemented"

    let fold f init d = failwith "Unimplemented"

    let format fmt d =
      (* Hint: use [format_assoc_list] as a helper. *)
      Format.fprintf fmt "<unimplemented>"
  end
