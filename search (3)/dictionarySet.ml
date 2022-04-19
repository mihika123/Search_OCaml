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
    module Val = struct
      type t = unit

      let format fmt () = Format.fprintf fmt ""
    end
    [@@coverage off]

    (** [t] is a dictionary set whose elements are keys of the
        dictionary. AF: [(k1, ()); (k2, ()) ... (kn, ())] is the set of
        keys {k1, k2, ... kn}. Their order and values are not relevant.
        There are no two keys that are bound to the same value in their
        mappings in the set. RI: There are no two mappings with the same
        key. *)
    module D = DM (E) (Val)

    type t = D.t

    (* [rep_ok s] checks if the representation invariant of [s] holds
       true. If [s] does not satisfy its rep invariants, it raises
       [Failure] *)
    let rep_ok s = D.rep_ok s [@@coverage off]

    (* [empty] is the empty dictionary set *)
    let empty = D.empty

    (* [is_empty s] returns the boolean result of comparing d with empty *)
    let is_empty s = D.is_empty s

    (* [size s] returns the size of the dictionary set *)
    let size s = D.size s

    (** [insert k s] is the set dictionary [s] with [k] as an element.
        If [k] was already present, its previous value is replaced with
        () *)
    let insert x s = D.insert x () s

    (** [member x s] checks if a binding to [x] exists in [s] and
        returns true if it does and false if it does not. *)
    let member x s = D.member x s

    (** [remove x s] contains all the elements of [s] except [x]. If [x]
        is not an element of [s], then [remove] returns a set with the
        same elements as [s]. *)
    let remove x s = D.remove x s

    (** [fold f init s] is [f xn (f ... (f x1 init) ...)], if [s]
        contains [x1]..[xn]. Elements are processed in order from least
        to greatest, where [x1] is the least element and [xn] is the
        greatest. *)
    let fold f init s = D.fold (fun k v a -> f k a) init s

    (** [union] is set union, that is, [union s1 s2] contains exactly
        those elements that are elements of [s1] or elements of [s2]. *)
    let union s1 s2 = s2 |> fold insert s1

    (* [helper l1] is a helper function that returns a list of all
       elements that present in both lists that are passed as parameters *)
    let rec helper l1 = function
      | [] -> []
      | h :: t ->
          if List.mem_assoc (fst h) l1 then h :: helper l1 t
          else helper l1 t

    (* [to_dict] is a helper function that converts a list to a set
       dictionary *)
    let rec to_dict = function
      | [] -> empty
      | h :: t -> insert (fst h) (to_dict t)

    (** [intersect] is set intersection, that is, [intersect s1 s2]
        contains exactly those elements that are elements of [s1] and
        elements of [s2]. *)
    let intersect s1 s2 =
      helper (D.to_list s1) (D.to_list s2) |> to_dict

    (** [diff_helper l1 l2] is a helper function that checks if an
        element in [l1] is also present in [l2] and adds it to the list
        it returns if not *)
    let rec diff_helper l1 l2 =
      match l1 with
      | [] -> []
      | h :: t ->
          if List.mem_assoc (fst h) l2 then diff_helper t l2
          else h :: diff_helper t l2

    (** [difference] is set difference, that is, [difference s1 s2]
        contains exactly those elements that are elements of [s1] but
        not elements of [s2]. *)
    let difference s1 s2 =
      diff_helper (D.to_list s1) (D.to_list s2) |> to_dict

    (*** [to_list s] is the set dictionary with keys sorted from least
      to greatest and with no dupliactes *)
    let to_list s = List.map (fun (elt, _) -> elt) (D.to_list s)

    (** [format] is a printing function suitable for use with the
        toplevel's [#install_printer] directive. It outputs a textual
        representation of a set on the given formatter. *)
    let format fmt d = D.format fmt d [@@coverage off] [@@coverage off]
  end
