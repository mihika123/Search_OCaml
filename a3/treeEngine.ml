(* Note that there is nothing you need to complete in this file. *)

open Dictionary

(** [S] is a dictionary set implemented with a [TreeDictionary] whose
    keys are strings. *)
module S = DictionarySet.Make (StringKey.String) (TreeDictionary.Make)

(** [D] is a [TreeDictionary] whose keys are strings. *)
module D = TreeDictionary.Make (StringKey.CaselessString) (S)

module TreeEngine = Engine.Make (S) (D)
