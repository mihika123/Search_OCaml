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

    (** [t] is a list that contains pairs of keys [key] and values
        [value]. AF: [(k1, v1); (k2, v2) ... (kn, vn)] is the dictionary
        {k1 : v1, k2 : v2 ... kn : vn}. The empty dictionary is
        represented by the empty list. RI: the list dictionary does not
        contain two or more entries with the same key *)
    type t = (key * value) list

    (*** [rep_ok d] checks if there are no two duplicate keys in the
      list dictionary. It returns [d] if [d] satisfies the rep
      invariant. If [d] does not satisfy its rep invariants, it raises
      [Failure] *)
    let rep_ok d =
      let compare_keys key1 key2 =
        match K.compare key1 key2 with LT -> -1 | GT -> 1 | EQ -> 0
      in
      let keys_list = List.split d |> fst in
      if
        keys_list |> List.length
        = (keys_list |> List.sort_uniq compare_keys |> List.length)
      then d
      else failwith "Rep invariant is not satisfied"
      [@@coverage off]

    (* [compare_keys key1 key2] compares two keys to check which is
       greater or lesser or equal *)
    let compare_keys key1 key2 =
      match K.compare key1 key2 with LT -> -1 | GT -> 1 | EQ -> 0

    (* [empty] is the empty list dictionary [] *)
    let empty = []

    (** [is_empty d] returns the boolean result of comparing d with
        empty *)
    let is_empty d = d = empty

    (** [size d] gives the number of key-value pairs in d. The size of
        the [empty] dictionary is 0 *)
    let size d = List.length d

    (** [remove k d] contains all pairs of the list dictionary [d]
        except the one with a value bound to [k]. If [d] doesn not have
        a binding for [k], [d] is returned. [remove_helper x] is a
        modified version of remove_assoc *)
    let remove k d =
      let rec remove_helper x = function
        | [] -> []
        | ((a, b) as pair) :: l ->
            if compare_keys a x = 0 then l
            else pair :: remove_helper x l
      in
      remove_helper k d

    (*** [find k d] returns the value that is bound to [k] in [d]. If
      there is no such value, it returns None. *)
    let find k d =
      let rec find_helper x = function
        | [] -> None
        | (a, b) :: l ->
            if compare_keys a x = 0 then Some b else find_helper x l
      in
      find_helper k d

    (*** [member k d] checks if a binding to [k] exists in [d] and
      returns true if it does and false if it does not. *)
    let member k d =
      let rec member_helper x = function
        | [] -> false
        | (a, b) :: l -> compare_keys a x = 0 || member_helper x l
      in
      member_helper k d

    (*** [insert k v d] is the list dictionary [d] with [k] bound to
      [v]. If [k] was already bound, its previous value is replaced with
      [v]. *)
    let insert k v d =
      let new_dict = if member k d then remove k d else d in
      (k, v) :: new_dict

    (*** [to_list d] is the list dictionary with keys sorted from least
      to greatest and with duplicates removed *)
    let to_list d =
      let keys = d |> List.map fst |> List.sort_uniq compare_keys in
      let rec dict_assoc x = function
        | [] -> raise Not_found
        | (a, b) :: l ->
            if compare_keys a x = 0 then b else dict_assoc x l
      in
      let f x = dict_assoc x d in
      let vals = keys |> List.map f in
      List.combine keys vals

    (* [fold f init d] is [f kn vn (f ... (f k1 v1 init) ...)], if [d]
       binds [ki] to [vi]. Bindings are processed in order from least to
       greatest, where [k1] is the least key and [kn] is the greatest. *)
    let fold f init d =
      List.fold_left (fun acc (k, v) -> f k v acc) init (to_list d)

    (* [format fmt d] gives a textual representation of a dictionary on
       the given formatter *)
    let format fmt d =
      (* Hint: use [format_assoc_list] as a helper. *)
      format_assoc_list K.format V.format fmt d
      [@@coverage off]
  end
