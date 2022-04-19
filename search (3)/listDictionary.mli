(** Dictionaries implemented as association lists. *)

open Dictionary

(** [Make] makes a [Dictionary] implemented with association lists. *)
module Make : DictionaryMaker

(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association
    list [lst] as a dictionary. The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively. The [fmt]
    argument is where to put the formatted output. *)
val format_assoc_list :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a * 'b) list ->
  unit
