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

    type color =
      | Red
      | Black

    (* t represents a red black tree *)
    (* AF: [Leaf] represents the empty set. [Node (c, v, l, r)]
       represents the set containing the value [v], and the elements of
       the sets represented by [l] and [r]. RI: The BST invariant holds,
       and the local and global RB tree invariants hold *)
    type t =
      | Node of color * (key * value) * t * t
      | Leaf

    let debug = false

    (** [checkValid k btype] returns true if the tree fulfils the
        conditions for a binary search tree, i.e., if for every node,
        the values of all nodes in the left subtree is lesser than the
        node and the values of all the nodes in the right subtree is
        greater than the node. [btype] checks if we are comparing the
        lower bound or the upper bound. *)
    let checkValid k btype = function
      | None -> true
      | Some x ->
          if btype = 1 then Key.compare x k = LT || Key.compare x k = EQ
          else Key.compare x k = GT || Key.compare x k = EQ
      [@@coverage off]

    (** [checkValidBST lo up] returns true if the tree fulfils the
        conditions for a binary search tree, i.e., if for every node,
        the values of all nodes in the left subtree is lesser than the
        node and the values of all the nodes in the right subtree is
        greater than the node. *)
    let rec checkValidBST lo up = function
      | Leaf -> true
      | Node (c, (k, v), l, r) ->
          if checkValid k 1 lo && checkValid k 0 up then
            checkValidBST lo (Some k) l && checkValidBST up (Some k) r
          else false
      [@@coverage off]

    (** [rbBlackValidCheck curr_height count] checks is the tree fulfils
        the RBTree invariant 2, every path from the root to a leaf has
        the same number of black nodes. This number is called the black
        height (BH) of the tree. *)
    let rec rbBlackValidCheck curr_height count = function
      | Leaf -> if curr_height = count then true else false
      | Node (c, (k, v), l, r) ->
          if c = Black then
            rbBlackValidCheck curr_height (count + 1) l
            && rbBlackValidCheck curr_height (count + 1) r
          else
            rbBlackValidCheck curr_height count l
            && rbBlackValidCheck curr_height count r
      [@@coverage off]

    (** [rbRedValidCheck curr_height count] checks is the tree fulfils
        the RBTree invariant 1, i.e., there are no two adjacent red
        nodes along any path.*)
    let rec rbRedValidCheck = function
      | Leaf -> true
      | Node (c, (_, _), l, r) -> (
          match (l, r) with
          | Leaf, Leaf -> true
          | (Node (c1, (_, _), _, _) as left), Leaf ->
              if c = Red && c1 = Red then false
              else rbRedValidCheck left
          | Leaf, (Node (c1, (_, _), _, _) as right) ->
              if c = Red && c1 = Red then false
              else rbRedValidCheck right
          | ( (Node (col1, (_, _), _, _) as left),
              (Node (col2, (_, _), _, _) as right) ) ->
              if c = Red && (col1 = Red || col2 = Red) then false
              else rbRedValidCheck left && rbRedValidCheck right )
      [@@coverage off]

    (** [rep_ok d] returns [d] if [d] satisfies its representation
        invariants for Red Black Trees. Raises: [Failure] with an
        unspecified error message if [d] does not satisfy its
        representation invariants. *)
    let rep_ok d =
      if debug then
        let rec blackCheck = function
          | Node (c, (k, v), l, r) -> (
              match c with
              | Black -> 1 + blackCheck l
              | _ -> blackCheck l )
          | Leaf -> 0
        in
        if
          checkValidBST None None d
          && rbRedValidCheck d
          && rbBlackValidCheck (blackCheck d) 0 d
        then d
        else failwith "Rep Invariant is not satisfied"
      else failwith "Rep Invariant is not satisfied"
      [@@coverage off]

    (* [empty] is the empty red black tree *)
    let empty = Leaf

    (** [is_empty d] returns the boolean result of comparing d with
        empty *)
    let is_empty d = d = empty

    (** [size d] gives the number of key-value pairs in d. The size of
        the [empty] tree is 0 *)
    let size d =
      let rec size_helper d count =
        match d with
        | Node (_, v, l, r) ->
            1 + size_helper l count + size_helper r count
        | Leaf -> count
      in
      size_helper d 0

    (* [balance] is a helper function that restores the rep invariant
       for red black trees when they are broken after inserting or
       removing nodes. *)
    let balance = function
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
          Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)

    (** [insert k v d] is [d] with [k] bound to [v]. If [k] was already
        bound, its previous value is replaced with [v]. *)
    let insert k v d =
      let rec ins = function
        | Leaf -> Node (Red, (k, v), Leaf, Leaf)
        | Node (color, (k1, v1), a, b) ->
            if Key.compare k k1 = LT then
              balance (color, (k1, v1), ins a, b)
            else if Key.compare k k1 = GT then
              balance (color, (k1, v1), a, ins b)
            else Node (color, (k, v), a, b)
      in
      match ins d with
      | Node (_, (k1, v1), a, b) -> Node (Black, (k1, v1), a, b)
      | Leaf ->
          (* guaranteed to be nonempty *)
          failwith "RBT insert failed with ins returning leaf"

    let remove k d = failwith "unimplemented" [@@coverage off]

    (** [find k d] is [Some v] if [k] is bound to [v] in [d]; or if [k]
        is not bound, then it is [None]. *)
    let find k d =
      let rec find_helper k d =
        match d with
        | Leaf -> None
        | Node (_, (k1, v1), l, r) ->
            if Key.compare k k1 = LT then find_helper k l
            else if Key.compare k k1 = GT then find_helper k r
            else Some v1
      in
      find_helper k d

    (** [member k d] is [true] iff [k] is bound in [d]. *)
    let member k d =
      let rec member_helper k d =
        match d with
        | Leaf -> false
        | Node (_, (k1, v1), l, r) ->
            if Key.compare k k1 = LT then member_helper k l
            else if Key.compare k k1 = GT then member_helper k r
            else true
      in
      member_helper k d

    (** [to_list d] is an association list containing the same bindings
        as [d]. The elements in the list are in order from the least key
        to the greatest. There are no duplicate keys in the list. *)
    let to_list d =
      let rec to_list_helper lst = function
        | Leaf -> lst
        | Node (_, (k, v), l, r) ->
            to_list_helper ((k, v) :: to_list_helper lst r) l
      in
      to_list_helper [] d

    (* [fold_helper f acc] is a helper function for the fold function in
       which a function is applied to bindings that are processed from
       the least to the greatest. *)
    let rec fold_helper f acc = function
      | [] -> acc
      | (k, v) :: t -> fold_helper f (f k v acc) t

    (** [fold f init d] is [f kn vn (f ... (f k1 v1 init) ...)], if [d]
        binds [ki] to [vi]. Bindings are processed in order from least
        to greatest, where [k1] is the least key and [kn] is the
        greatest. *)
    let fold f acc d = fold_helper f acc (to_list d)

    (** [format_assoc_list fmt_key fmt_val fmt lst] formats an
        association list [lst] as a dictionary. The [fmt_key] and
        [fmt_val] arguments are formatters for the key and value types,
        respectively. The [fmt] argument is where to put the formatted
        output. *)
    let format_assoc_list format_key format_val fmt lst =
      (* You are free to improve the output of this function in any way
         you like. *)
      Format.fprintf fmt "[";
      List.iter
        (fun (k, v) ->
          Format.fprintf fmt "%a -> %a; " format_key k format_val v)
        lst;
      Format.fprintf fmt "]"
      [@@coverage off]

    (** [format] is a printing function suitable for use with the
        toplevel's [#install_printer] directive. It outputs a textual
        representation of a dictionary on the given formatter. *)
    let format fmt d =
      format_assoc_list K.format V.format fmt (to_list d)
      [@@coverage off]
  end
