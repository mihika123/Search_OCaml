(* Note that there is nothing you need to complete in this file. All the
   work you need to do for engines is in [engine.ml]. *)

open Dictionary

(** [S] is a dictionary set implemented with a [ListDictionary] whose
    keys are strings. *)
module S = DictionarySet.Make (StringKey.String) (ListDictionary.Make)

(** [D] is a [ListDictionary] whose keys are caseless strings. *)
module D = ListDictionary.Make (StringKey.CaselessString) (S)

module ListEngine = Engine.Make (S) (D)
