module type Engine = sig
  type idx

  val index_of_dir : string -> idx

  val words : idx -> string list

  val to_list : idx -> (string * string list) list

  val or_not : idx -> string list -> string list -> string list

  val and_not : idx -> string list -> string list -> string list

  val format : Format.formatter -> idx -> unit
end

module Make =
functor
  (S : DictionarySet.Set with type Elt.t = string)
  (D : Dictionary.Dictionary
         with type Key.t = string
          and type Value.t = S.t)
  ->
  struct
    (* Begin: Do not change any of the provided code below. *)

    (** An [idx] maps from strings to sets of strings. *)
    type idx = D.t

    (** [word_regexp] is a regular expression for parsing words. It
        matches strings that begin and end with a _boundary character_,
        which is any lowercase letter, uppercase letter, or digit. In
        between the boundary characters there may be any number and kind
        of other characters. There are some weird corner cases resulting
        from this definition of words, but it's relatively simple, and
        it gets many common cases right.*)
    let word_regexp =
      Str.regexp "\\([A-Za-z0-9]+.*[A-Za-z0-9]+\\)\\|[A-Za-z0-9]+"

    (** [word_of_preword pw] is the lower-case _word_ corresponding to
        _preword_ [pw], if any. A preword is a sequence of
        non-blank-space characters. See [word_regexp] for the definition
        of _word_. *)
    let word_of_preword pw =
      let open Str in
      try
        let _ = search_forward word_regexp pw 0 in
        Some (String.lowercase_ascii (matched_string pw))
      with Not_found -> None

    (** [words_of_prewords] converts a list of prewords to a list of
        words. *)
    let words_of_prewords pws =
      (* we do this with a custom function, so that we can
         simultaneously map and filter, tail recursively, while not
         allocating any more memory than needed, and so that we
         hopefully don't trigger too much GC when processing large files *)
      let rec loop acc = function
        | [] -> acc
        | h :: t -> (
            match word_of_preword h with
            | None -> loop acc t
            | Some w -> loop (w :: acc) t )
      in
      loop [] pws

    (** [blankspace_regexp] is a regular expression that matches _blank
        space_, which are spaces and tabs. *)
    let blankspace_regexp = Str.regexp "[ \t]+"

    (** [words_in_line line] is a list of the words parsed from a line
        of text. *)
    let words_in_line line =
      line
      |> (* extract prewords *) Str.split blankspace_regexp
      |> (* convert to words *) words_of_prewords

    (** [safe_input_line] inputs a line from an input channel, without
        raising an exception. *)
    let safe_input_line chan =
      try Some (input_line chan) with End_of_file -> None

    (** [insert_bindings words file dict] iterates over every word [w]
        in [words], inserting [file] into the set [s] to which [dict]
        binds [w]. *)
    let insert_bindings words file dict =
      let wordset word dict =
        match D.find word dict with None -> S.empty | Some s -> s
      in
      let insert_binding dict word =
        let s = wordset word dict in
        let s' = S.insert file s in
        D.insert word s' dict
      in
      List.fold_left insert_binding dict words

    (** [index_of_file path name d] indexes the file found at [path],
        adding bindings for its words to [name] in dictionary [d]. *)
    let index_of_file filepath filename dict =
      let chan = open_in filepath in
      let rec loop acc =
        match safe_input_line chan with
        | None -> acc
        | Some line ->
            let word_list = words_in_line line in
            loop (insert_bindings word_list filename acc)
      in
      let dict' = loop dict in
      close_in chan;
      dict'

    let index_of_dir d =
      let safe_readdir dh =
        try Some (Unix.readdir dh) with End_of_file -> None
      in
      let my_opendir d =
        try Unix.opendir d with _ -> raise Not_found
      in
      let is_txt_file f = Filename.check_suffix f ".txt" in
      let dh = my_opendir d in
      let rec loop acc =
        match safe_readdir dh with
        | Some f ->
            if is_txt_file f then
              let fp = d ^ Filename.dir_sep ^ f in
              loop (index_of_file fp f acc)
            else loop acc
        | None -> acc
      in
      let dict = loop D.empty in
      Unix.closedir dh;
      dict

    (* End: Do not change any of the provided code above. *)

    (** [to_list idx] is a representation of [idx] as an association
        list. Order in the list is irrelevant. The keys in the list must
        be equivalent to [words idx], ignoring order. The value
        corresponding to a key is a set-like list of the files in which
        that word appears. Regarding the case of keys in the list, see
        [words]. *)
    let to_list idx =
      let f k v acc = (k, S.to_list v) :: acc in
      idx |> D.fold f [] |> List.rev

    (** [words idx] is a set-like list of the words indexed in [idx]. If
        a word occurs with multiple cases in the file, no guarantee is
        made about which case is returned except that it will be exactly
        one of them. Example: if a file contains "OCaml" and "OCAML" and
        "ocaml", exactly one of of those will appear in the list, but it
        is unspecified which. *)
    let words idx = idx |> D.to_list |> List.split |> fst

    (* [to_lc] chnages all the strings in the list to lowercase and then
       returns it. *)
    let rec to_lc = function
      | [] -> []
      | h :: t -> String.lowercase_ascii h :: to_lc t

    (** [op_on_files idx f lst] is a helper function that applies the
        function union or intersect on the sets depending on whether it
        is called by or_not aor and_not. *)
    let op_on_files idx f lst =
      let f1 k v acc = if List.mem k lst then f v acc else acc in
      idx |> D.fold f1 S.empty

    (** [or_not idx ors nots] is a set-like list of the files that
        contain any of the words in [ors] and none of the words in
        [nots]. The case of words in [ors] and [nots] is irrelevant.
        Example: a query with [ors = "OCaml"] will return the same as
        [ors = "ocaml"] and [ors = "OCAML"]. Requires: [ors] is not
        empty. *)
    let or_not idx ors nots =
      op_on_files idx S.union (to_lc nots)
      |> S.difference (op_on_files idx S.union (to_lc ors))
      |> S.to_list

    (* [check_for_ands idx ands] checks to see if [v] exist in [idx] and
       if it doesn't it resturns the accumulator and then calls
       fold_left on it to apply the function f on bindings from the
       right to the left *)
    let check_for_ands idx ands =
      let f acc v = if D.find v idx = None then false else acc in
      ands |> List.fold_left f true

    (** [and_not idx ands nots] is a set-like list of the files that
        contain all of the words in [ands] and none of the words in
        [nots]. The case of words in [ands] and [nots] is irrelevant.
        Example: a query with [ands = "OCaml"] will return the same as
        [ands = "ocaml"] and [ands = "OCAML"]. Requires: [ands] is not
        empty. *)
    let and_not idx ands nots =
      if check_for_ands idx ands then
        op_on_files idx S.union (to_lc nots)
        |> S.difference (op_on_files idx S.intersect (to_lc ands))
        |> S.to_list
      else []

    (** [format] is a printing function suitable for use with the
        toplevel's [#install_printer] directive. It outputs a textual
        representation of an index on the given formatter. *)
    let format fmt idx = D.format fmt idx
  end
