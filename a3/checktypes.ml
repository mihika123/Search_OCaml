(******************************************************************)
(* DO NOT EDIT THIS FILE                                          *)
(******************************************************************)

module type DictionarySig = sig
  type order =
    | LT
    | EQ
    | GT

  module type Comparable = sig
    type t

    val compare : t -> t -> order
  end

  module type Formattable = sig
    type t

    val format : Format.formatter -> t -> unit
  end

  module type KeySig = sig
    type t

    val compare : t -> t -> order

    val format : Format.formatter -> t -> unit
  end

  module type ValueSig = sig
    type t

    val format : Format.formatter -> t -> unit
  end

  module type Dictionary = sig
    module Key : KeySig

    module Value : ValueSig

    type key = Key.t

    type value = Value.t

    type t

    val rep_ok : t -> t

    val empty : t

    val is_empty : t -> bool

    val size : t -> int

    val insert : key -> value -> t -> t

    val member : key -> t -> bool

    val find : key -> t -> value option

    val remove : key -> t -> t

    val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc

    val to_list : t -> (key * value) list

    val format : Format.formatter -> t -> unit
  end

  module type DictionaryMaker = functor (K : KeySig) (V : ValueSig) ->
    Dictionary with module Key = K and module Value = V
end

module DictionaryCheck : DictionarySig = Dictionary

module type DictionarySetSig = sig
  module type ElementSig = sig
    type t

    val compare : t -> t -> Dictionary.order

    val format : Format.formatter -> t -> unit
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

  module Make : functor
    (E : ElementSig)
    (DM : Dictionary.DictionaryMaker)
    -> Set with module Elt = E
end

module DictionarySetCheck : DictionarySetSig = DictionarySet

module type ListDictionarySig = sig
  module Make : Dictionary.DictionaryMaker
end

module CheckListDictionary : ListDictionarySig = ListDictionary

module type TreeDictionarySig = sig
  module Make : Dictionary.DictionaryMaker
end

module CheckTreeDictionary : TreeDictionarySig = TreeDictionary

module type Engine = sig
  type idx

  val index_of_dir : string -> idx

  val words : idx -> string list

  val to_list : idx -> (string * string list) list

  val or_not : idx -> string list -> string list -> string list

  val and_not : idx -> string list -> string list -> string list

  val format : Format.formatter -> idx -> unit
end

module type EngineSig = sig
  module type Engine = Engine

  module Make : functor
    (S : DictionarySet.Set with type Elt.t = string)
    (D : Dictionary.Dictionary
           with type Key.t = string
            and type Value.t = S.t)
    -> Engine
end

module EngineCheck : EngineSig = Engine

module type ListEngineSig = sig
  module ListEngine : Engine
end

module ListEngineCheck : ListEngineSig = ListEngine

module type TreeEngineSig = sig
  module TreeEngine : Engine
end

module TreeEngineCheck : TreeEngineSig = TreeEngine

module type AuthorsSig = sig
  val hours_worked : int list
end

module AuthorsCheck : AuthorsSig = Authors
