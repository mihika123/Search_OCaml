open OUnit2
open Dictionary
open ListDictionary
open DictionarySet
open TreeDictionary
open ListEngine
open TreeEngine

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. It is taken from A2 *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_int s] pretty-prints string [s]. It is taken from A2. *)
let pp_int s = "\"" ^ string_of_int s ^ "\""

(* [valFromOpt] extracts the value from the option. If the option is
   None, it returns -1. *)
let valFromOpt = function None -> -1 | Some x -> x

module KeysTest = struct
  type t = int

  let compare t1 t2 =
    match Stdlib.compare t1 t2 with
    | t1 when t1 < 0 -> LT
    | 0 -> EQ
    | _ -> GT

  let format fmt k = Format.fprintf fmt "%d" k
end

module ValuesTest = struct
  type t = int

  let format fmt v = Format.fprintf fmt "%d" v
end

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. Borrowed from A2*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

module ListDict = ListDictionary.Make (KeysTest) (ValuesTest)

let e = ListDict.empty

let dict1 = ListDict.insert 6 7 e

let dict2 = ListDict.insert 8 2 dict1

let dict3 = ListDict.insert 3 3 dict2

let dict4 = ListDict.insert 1 2 dict3

let dict5 = ListDict.insert 8 3 dict2

let func1 init k v = k + v + init

let func2 init k v = init + k - v

let ldTests =
  [
    ( "Test for is_empty with empty dict" >:: fun _ ->
      assert_equal (ListDict.is_empty e) true );
    ( "Test for is_empty with non-empty dict" >:: fun _ ->
      assert_equal (ListDict.is_empty dict1) false );
    ( "Test for size on empty dict" >:: fun _ ->
      assert_equal (ListDict.size e) 0 );
    ( "Size on non-empty dict" >:: fun _ ->
      assert_equal (ListDict.size dict2) 2 );
    ( "Test for insert from dict1" >:: fun _ ->
      assert_equal (ListDict.insert 6 7 e) dict1 );
    ( "Test for insert from dict2" >:: fun _ ->
      assert_equal (ListDict.insert 8 2 dict1) dict2 );
    ( "Test for insert when key is already bound" >:: fun _ ->
      assert_equal (ListDict.insert 8 3 dict2) dict5 );
    ( "Basic test for remove" >:: fun _ ->
      assert_equal (ListDict.remove 6 dict1) e );
    ( "Test for remove when key is not bound" >:: fun _ ->
      assert_equal (ListDict.remove 9 dict3) dict3 );
    ( "Basic test for find" >:: fun _ ->
      assert_equal (ListDict.find 6 dict2) (Some 7) );
    ( "Test for find with key not in dict" >:: fun _ ->
      assert_equal (ListDict.find 15 dict4) None );
    ( "Test for find with key not in dict for duplicates" >:: fun _ ->
      assert_equal (ListDict.find 8 dict5) (Some 3) );
    ( "Basic test for member" >:: fun _ ->
      assert_equal (ListDict.member 6 dict2) true );
    ( "Test for member when key not in dict" >:: fun _ ->
      assert_equal (ListDict.member 15 dict4) false );
    ( "Test for member when key not in dict for duplicates" >:: fun _ ->
      assert_equal (ListDict.member 8 dict5) true );
    ( "Empty list test for to_list" >:: fun _ ->
      assert_equal (ListDict.to_list e) [] );
    ( "General test for to_list" >:: fun _ ->
      assert_equal
        (ListDict.to_list dict4)
        [ (1, 2); (3, 3); (6, 7); (8, 2) ] );
    ( "General test for fold with f given above" >:: fun _ ->
      assert_equal (ListDict.fold func1 0 dict1) 13 );
    ( "General test for fold on empty dict" >:: fun _ ->
      assert_equal (ListDict.fold func1 5 e) 5 );
  ]

module ElementTest = struct
  type t = int

  let compare t1 t2 =
    match Stdlib.compare t1 t2 with
    | t1 when t1 < 0 -> LT
    | 0 -> EQ
    | _ -> GT

  let format fmt e = Format.fprintf fmt "%d" e
end

module DictSet = DictionarySet.Make (ElementTest) (ListDictionary.Make)

let em = DictSet.empty

let set1 = DictSet.insert 5 em (* {5} *)

let set2 = DictSet.insert 7 set1 (* {5, 7} *)

let set3 = DictSet.insert 2 set2 (* {5, 7, 2} *)

let set4 = DictSet.insert 5 em (* {5} *)

let set5 = DictSet.insert 1 set4 (* {5, 1} *)

let set6 = DictSet.insert 6 em (* {6} *)

let set7 = DictSet.insert 6 set4 (* {5, 6} *)

let func3 init k = k + init

let func4 init k = k - init

(** [intersect_test name l1 l2 output] constructs an OUnit test named
    [name] that asserts the equality of [output] with
    [DictSet.intersect l1 l2]. *)
let intersect_test name l1 l2 output =
  name >:: fun _ ->
  assert_equal output
    (DictSet.to_list (DictSet.intersect l1 l2))
    ~printer:(pp_list pp_int)

(** [diff_test name l1 l2 output] constructs an OUnit test named [name]
    that asserts the equality of [output] with
    [DictSet.difference l1 l2]. *)
let diff_test name l1 l2 output =
  name >:: fun _ ->
  assert_equal output
    (DictSet.to_list (DictSet.difference l1 l2))
    ~printer:(pp_list pp_int)

let dsTests =
  [
    ( "Test for is_empty with empty set" >:: fun _ ->
      assert_equal (DictSet.is_empty em) true );
    ( "Test for is_empty with non-empty set" >:: fun _ ->
      assert_equal (DictSet.is_empty set1) false );
    ( "Test for size on empty dict set" >:: fun _ ->
      assert_equal (DictSet.size em) 0 );
    ( "Size on non-empty dict set" >:: fun _ ->
      assert_equal (DictSet.size set2) 2 );
    ( "Test for insert from set1" >:: fun _ ->
      assert_equal (DictSet.insert 5 em) set1 );
    ( "Basic test for remove" >:: fun _ ->
      assert_equal (DictSet.remove 7 set2) set1 );
    ( "Test for remove when elt is not there" >:: fun _ ->
      assert_equal (DictSet.remove 9 set5) set5 );
    ( "Basic test for member" >:: fun _ ->
      assert_equal (DictSet.member 5 set1) true );
    ( "Test for member when key not in set" >:: fun _ ->
      assert_equal (DictSet.member 15 em) false );
    ( "Empty list test for to_list" >:: fun _ ->
      assert_equal (DictSet.to_list em) [] );
    ( "General test for to_list" >:: fun _ ->
      assert_equal (DictSet.to_list set3) [ 2; 5; 7 ] );
    ( "General test for fold with f given above" >:: fun _ ->
      assert_equal (DictSet.fold func3 0 set5) 6 );
    ( "General test for fold on empty set" >:: fun _ ->
      assert_equal (DictSet.fold func4 (-2) em) (-2) );
    ( "General union test with no same elements" >:: fun _ ->
      assert_equal (DictSet.union set1 set6) set7 );
    ( "union test with overlapping elements" >:: fun _ ->
      assert_equal (DictSet.union set1 set2) set2 );
    ( "union test with empty test" >:: fun _ ->
      assert_equal (DictSet.union set4 em) set4 );
    intersect_test "General intersect test with no overlapping elements"
      set1 set6 [];
    intersect_test "Intersect test with overlapping elements" set1 set2
      [ 5 ];
    ( "Intersect on the empty set" >:: fun _ ->
      assert_equal (DictSet.intersect set5 em) em );
    diff_test "Diff test for no overlapping elements" set6 set1 [ 6 ];
    diff_test "Diff test for some overlapping elements" set7 set5 [ 6 ];
    diff_test "Diff test for all overlapping elements" set1 set4 [];
    diff_test
      "Diff test for sets when first set is a subset of second set" set2
      set3 [];
  ]

module RBTree = TreeDictionary.Make (KeysTest) (ValuesTest)

let emp = RBTree.empty

let tree1 = RBTree.insert 8 9 emp

let tree2 = RBTree.insert 7 3 tree1

let tree3 = RBTree.insert 10 12 tree2

let tree4 = RBTree.insert 5 20 tree3

let tree5 = RBTree.insert 9 3 tree4

let tree6 = RBTree.insert 6 10 tree5

let tree7 = RBTree.insert 12 12 tree6

let tree8 = RBTree.insert 10 10 tree5

let func5 k v init = init + k + v

let func6 k v init = init + k - v

(** [fold_test name f acc d output] constructs an OUnit test named
    [name] that asserts the equality of [output] with
    [RBTree.fold f acc d]. *)
let fold_test name f acc d output =
  name >:: fun _ ->
  assert_equal output (RBTree.fold f acc d) ~printer:pp_int

let rbTests =
  [
    ( "Test for is_empty with empty tree" >:: fun _ ->
      assert_equal (RBTree.is_empty emp) true );
    ( "Test for is_empty with non-empty set" >:: fun _ ->
      assert_equal (RBTree.is_empty tree1) false );
    ( "Test for size for empty red black tree set" >:: fun _ ->
      assert_equal (RBTree.size emp) 0 );
    ( "Test for size for non-empty red black tree set" >:: fun _ ->
      assert_equal (RBTree.size tree2) 2 );
    ( "Test for insert from tree4" >:: fun _ ->
      assert_equal (RBTree.insert 9 3 tree4) tree5 );
    ( "Test for insert from tree5" >:: fun _ ->
      assert_equal (RBTree.insert 10 10 tree5) tree8 );
    ( "Basic test for find" >:: fun _ ->
      assert_equal (RBTree.find 7 tree2) (Some 3) );
    ( "Test for find with key not in tree" >:: fun _ ->
      assert_equal (RBTree.find 15 tree3) None );
    ( "Basic test for member" >:: fun _ ->
      assert_equal (RBTree.member 10 tree3) true );
    ( "Test for member when key not in tree" >:: fun _ ->
      assert_equal (RBTree.member 15 emp) false );
    ( "Test for to_list with empty tree" >:: fun _ ->
      assert_equal (RBTree.to_list emp) [] );
    ( "Test for to_list with tree2" >:: fun _ ->
      assert_equal (RBTree.to_list tree2) [ (7, 3); (8, 9) ] );
    ( "Test for fold for empty tree" >:: fun _ ->
      assert_equal (RBTree.fold func5 7 emp) 7 );
    fold_test "Test for tree after inserting dupliactes" func5 0 tree8
      84;
    fold_test "Test for fold for non-empty tree" func6 0 tree3 1;
    fold_test "Test for fold for non-empty tree" func6 0 tree4 (-14);
  ]

let test_suite = List.flatten [ ldTests; dsTests; rbTests ]

let suite = "search test suite" >::: test_suite

let _ = run_test_tt_main suite
