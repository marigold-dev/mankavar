(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <gabriel.alfour@gmail.com>                    *)
(* Copyright (c) 2021 Nomadic Labs. <locascio.Antonio@nomadic-labs.com>      *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Util
open Tezos_traced_storage.Sparse

module THash = Tezos_traced_storage.Traced_hash

module Patricia_unit = struct
  open Patricia
  open Util.Dev

  let test_get_raw =
    test "patricia-get-raw" @@ fun () ->
    (* Valid get *)
    assert (snd @@ get raw_t1 [false; false] = Bytes.of_string "x") ;
    assert (snd @@ get raw_t1 [false; true; false] = Bytes.of_string "y") ;
    assert (snd @@ get raw_t1 [true] = Bytes.of_string "z") ;
    assert_none (fun () -> snd @@ get_opt raw_t1 [false; true; true]) ;
    (* Invalid get: longer key on Empty *)
    assert_none (fun () -> snd @@ get_opt raw_t1 [false; true; true; true]) ;
    (* Invalid get: key ends in Node *)
    assert_invalid_key (fun () -> snd @@ get_opt raw_t1 []) ;
    assert_invalid_key (fun () -> snd @@ get_opt raw_t1 [false; true]) ;
    (* Invalid get: longer key on Leaf (a prefix of the key exists) *)
    assert_invalid_key (fun () -> snd @@ get_opt raw_t1 [false; false; false]) ;
    assert_invalid_key (fun () -> snd @@ get_opt raw_t1 [true; true])

  let test_set_raw =
    test "patricia-set-raw" @@ fun () ->
    (* Valid sets *)
    assert (set raw_t1 [false; true; true] (Bytes.of_string "w") = raw_t2) ;
    assert (set raw_t1 [false; true; true; true] (Bytes.of_string "w") = raw_t3) ;
    (* Failing set on Leaf: a prefix of the new key belongs to the tree *)
    assert_invalid_key (fun () ->
        set raw_t1 [false; false; true] (Bytes.of_string "w")) ;
    (* Failing set on Node *)
    assert_invalid_key (fun () ->
        set raw_t1 [false; true] (Bytes.of_string "w"))

  let test_set =
    test "patricia-set" @@ fun () ->
    (* Valid set on Empty *)
    let t = set empty key_a value_x in
    assert (snd @@ get t key_a = value_x) ;
    (* Valid set on Leaf *)
    let t = set t key_a value_y in
    assert (snd @@ get t key_a = value_y) ;
    (* Failing set on Node: we try to insert a key that is a prefix of
       an existing key *)
    (* key_c is a prefix of key_d *)
    let t' = set t key_d value_z in
    assert_invalid_key (fun () -> set t' key_c value_x) ;
    (* Failing set on Leaf: a prefix of the new key belongs to the tree *)
    let t'' = set t key_c value_z in
    assert_invalid_key (fun () -> set t'' key_d value_x)

  let test_mem =
    test "patricia-mem" @@ fun () ->
    let t = empty in
    (* Invalid mem on empty tree *)
    assert (not @@ snd @@ mem t key_a) ;
    assert (not @@ snd @@ mem t key_b) ;
    (* Valid mem *)
    let t = set t key_a value_x in
    assert (snd @@ mem t key_a) ;
    (* Invalid mem on non-empty tree *)
    assert (not @@ snd @@ mem t key_b)

  let test_get =
    test "patricia-get" @@ fun () ->
    let t = empty in

    (* Valid get *)
    let t = set t key_a value_x in
    assert (snd @@ get t key_a = value_x) ;
    (* Invalid get: longer key on Empty *)
    assert_none (fun () -> snd @@ get_opt t key_b) ;
    assert_none (fun () -> snd @@ get_opt t key_c) ;

    (* Valid gets after modifying another keys*)
    let t = set t key_b value_y in
    assert (snd @@ get t key_a = value_x) ;
    assert (snd @@ get t key_b = value_y) ;
    assert_none (fun () -> snd @@ get_opt t key_c) ;
    let t = set t key_c value_z in
    assert (snd @@ get t key_a = value_x) ;
    assert (snd @@ get t key_c = value_z) ;

    (* Invalid get: key ends in Node *)
    assert_invalid_key (fun () -> get t []) ;
    (* Invalid get: key ends in Empty *)
    assert_not_found (fun () -> get empty []) ;
    (* Invalid get: longer key on Leaf (a prefix of the key exists) *)
    assert_invalid_key (fun () -> get t key_d)

  let tests = [test_get_raw; test_set_raw; test_set; test_mem; test_get]
end

module Produce_unit = struct
  open Patricia_produce_stream
  open Util.Dev

  let test_get_raw =
    let open Patricia_produce_stream_impl in
    let raw_t1 = of_tree raw_t1 in
    let s = Stream.Producer.empty in
    test "patricia-produce-get-raw" @@ fun () ->
    (* Valid gets *)
    let ((t', s'), v) = get (raw_t1, s) [false; false] in
    assert (v = Bytes.of_string "x") ;
    (* Check stream *)
    assert (s' = Stream.Producer.Raw.of_list stream_t1_ll) ;
    (* Check visited tree *)
    assert (t' = visited_t1_ll) ;
    assert (snd @@ get (raw_t1, s) [false; true; false] = Bytes.of_string "y") ;
    assert (snd @@ get (raw_t1, s) [true] = Bytes.of_string "z") ;
    assert_none (fun () -> snd @@ get_opt (raw_t1, s) [false; true; true]) ;
    (* Invalid get: longer key on Empty *)
    assert_none (fun () -> snd @@ get_opt (raw_t1, s) [false; true; true; true]) ;
    (* Invalid get: key ends in Node *)
    assert_invalid_key (fun () -> snd @@ get_opt (raw_t1, s) []) ;
    assert_invalid_key (fun () -> snd @@ get_opt (raw_t1, s) [false; true]) ;
    (* Invalid get: longer key on Leaf (a prefix of the key exists) *)
    assert_invalid_key (fun () ->
        snd @@ get_opt (raw_t1, s) [false; false; false]) ;
    assert_invalid_key (fun () -> snd @@ get_opt (raw_t1, s) [true; true])

  let test_set_raw =
    test "patricia-produce-set-raw" @@ fun () ->
    let open Patricia_produce_stream_impl in
    (* Valid set *)
    let raw_t1 = of_tree raw_t1 in
    let s = Stream.Producer.empty in
    let (t', s') = set (raw_t1, s) [false; true; true] (Bytes.of_string "w") in
    assert (t' = visited_raw_t2) ;
    assert (s' = Stream.Producer.Raw.of_list stream_t2_lrr)

  let test_set =
    test "produce-set" @@ fun () ->
    (* Valid set on Empty *)
    let t = set (empty, Stream.Producer.empty) key_a value_x in
    assert (snd @@ get t key_a = value_x) ;
    (* Valid set on Leaf *)
    let t = set t key_a value_y in
    assert (snd @@ get t key_a = value_y) ;
    (* Failing set on Node: we try to insert a key that is a prefix of
       an existing key *)
    (* key_c is a prefix of key_d *)
    let t' = set t key_d value_z in
    assert_invalid_key (fun () -> set t' key_c value_x) ;
    (* Failing set on Leaf: a prefix of the new key belongs to the tree *)
    let t'' = set t key_c value_z in
    assert_invalid_key (fun () -> set t'' key_d value_x)

  let test_mem =
    test "produce-mem" @@ fun () ->
    let t = (empty, Stream.Producer.empty) in
    (* Invalid mem on empty tree *)
    assert (not @@ snd @@ mem t key_a) ;
    assert (not @@ snd @@ mem t key_b) ;
    (* Valid mem *)
    let t = set t key_a value_x in
    assert (snd @@ mem t key_a) ;
    (* Invalid mem on non-empty tree *)
    assert (not @@ snd @@ mem t key_b)

  let test_get =
    test "produce-get" @@ fun () ->
    let t = (empty, Stream.Producer.empty) in

    (* Valid get *)
    let t = set t key_a value_x in
    assert (snd @@ get t key_a = value_x) ;
    (* Invalid get: longer key on Empty *)
    assert_none (fun () -> snd @@ get_opt t key_b) ;
    assert_none (fun () -> snd @@ get_opt t key_c) ;

    (* Valid gets after modifying another keys*)
    let t = set t key_b value_y in
    assert (snd @@ get t key_a = value_x) ;
    assert (snd @@ get t key_b = value_y) ;
    assert_none (fun () -> snd @@ get_opt t key_c) ;
    let t = set t key_c value_z in
    assert (snd @@ get t key_a = value_x) ;
    assert (snd @@ get t key_c = value_z) ;

    (* Invalid get: key ends in Node *)
    assert_invalid_key (fun () -> get t []) ;
    (* Invalid get: key ends in Empty *)
    assert_not_found (fun () -> get (empty, Stream.Producer.empty) []) ;
    (* Invalid get: longer key on Leaf (a prefix of the key exists) *)
    assert_invalid_key (fun () -> get t key_d)

  let produce_store =
    test "produce-store" @@ fun () ->
    let open Patricia_produce_stream in
    let s = Stream.Producer.empty in
    let t = dummy_produce () in

    let ((t, s), value_a) = get (t, s) key_a in
    assert (value_a = value_x) ;
    assert_none (fun () -> snd @@ get_opt (t, s) key_b) ;
    let ((_t, _s), value_c) = get (t, s) key_c in
    assert (value_c = value_z)

  let tests =
    [test_get_raw; test_set_raw; test_set; test_mem; test_get; produce_store]
end

module Consume_unit = struct
  open Patricia_consume_stream
  open Util.Dev

  let empty_s = (empty, Stream.Consumer.empty)

  let test_get_raw =
    let open Patricia_consume_stream_impl in
    let t = Hash (Tree.get_hash raw_t1) in
    let s =
      Stream.Producer.to_consumer (Stream.Producer.Raw.of_list stream_t1_ll)
    in
    test "patricia-consume-get-raw" @@ fun () ->
    (* Valid get *)
    let ((t', s'), v) = get (t, s) [false; false] in
    assert (v = Bytes.of_string "x") ;
    (* Check stream *)
    assert (s' = Stream.Consumer.empty) ;
    (* Check tree *)
    assert (t' = th_t1_ll)

  let test_set_raw =
    test "patricia-consume-set-raw" @@ fun () ->
    let open Patricia_consume_stream_impl in
    let t = Hash (Tree.get_hash raw_t1) in
    let s =
      Stream.Producer.to_consumer (Stream.Producer.Raw.of_list stream_t2_lrr)
    in
    (* Valid set *)
    let (t', s') = set (t, s) [false; true; true] (Bytes.of_string "w") in
    assert (t' = th_t2_lrr) ;
    assert (s' = Stream.Consumer.empty)

  let test_set =
    test "consume-set" @@ fun () ->
    (* Valid set on Empty *)
    let t = set empty_s key_a value_x in
    assert (snd @@ get t key_a = value_x) ;
    (* Valid set on Leaf *)
    let t = set t key_a value_y in
    assert (snd @@ get t key_a = value_y) ;
    (* Failing set on Node: we try to insert a key that is a prefix of
       an existing key *)
    (* key_c is a prefix of key_d *)
    let t' = set t key_d value_z in
    assert_invalid_key (fun () -> set t' key_c value_x) ;
    (* Failing set on Leaf: a prefix of the new key belongs to the tree *)
    let t'' = set t key_c value_z in
    assert_invalid_key (fun () -> set t'' key_d value_x)

  let test_mem =
    test "consume-mem" @@ fun () ->
    let t = empty_s in
    (* Invalid mem on empty tree *)
    assert (not @@ snd @@ mem t key_a) ;
    assert (not @@ snd @@ mem t key_b) ;
    (* Valid mem *)
    let t = set t key_a value_x in
    assert (snd @@ mem t key_a) ;
    (* Invalid mem on non-empty tree *)
    assert (not @@ snd @@ mem t key_b)

  let test_get =
    test "consume-get" @@ fun () ->
    let t = empty_s in

    (* Valid get *)
    let t = set t key_a value_x in
    assert (snd @@ get t key_a = value_x) ;
    (* Invalid get: longer key on Empty *)
    assert_none (fun () -> snd @@ get_opt t key_b) ;
    assert_none (fun () -> snd @@ get_opt t key_c) ;

    (* Valid gets after modifying another keys*)
    let t = set t key_b value_y in
    assert (snd @@ get t key_a = value_x) ;
    assert (snd @@ get t key_b = value_y) ;
    assert_none (fun () -> snd @@ get_opt t key_c) ;
    let t = set t key_c value_z in
    assert (snd @@ get t key_a = value_x) ;
    assert (snd @@ get t key_c = value_z) ;

    (* Invalid get: key ends in Node *)
    assert_invalid_key (fun () -> get t []) ;
    (* Invalid get: key ends in Empty *)
    assert_not_found (fun () -> get empty_s []) ;
    (* Invalid get: longer key on Leaf (a prefix of the key exists) *)
    assert_invalid_key (fun () -> get t key_d)

  let dummy_stream () : Stream.Producer.t =
    let open Patricia_produce_stream in
    let s = Stream.Producer.empty in
    let t = dummy_produce () in
    let ((t, s), _) = get (t, s) key_a in
    let (t, s) = set (t, s) key_e value_y in
    let ((t, s), _) = get (t, s) key_c in
    ignore t ;
    s

  (* Dummy produce originally is: [key_a -> value_x; key_c -> value_z] *)
  let nuke_tree =
    test "consume-nuke-tree" @@ fun () ->
    let open Patricia_consume_stream in
    let h =
      Patricia_produce_stream.get_hash @@ (dummy_produce (), dummy_stream ())
    in
    let t = root_hash h in
    (* It's important that the first set fails, as otherwise it erases the whole tree. *)
    assert_bad_stream (fun () ->
        let (t, s) = set (t, Stream.Consumer.empty) [] value_x in
        let _ = get_opt (t, s) key_a in
        ignore @@ get_opt (t, s) key_c)

  let consume_store =
    test "consume-store" @@ fun () ->
    let open Patricia_consume_stream in
    let s = Stream.Producer.to_consumer @@ dummy_stream () in
    let h =
      Patricia_produce_stream.get_hash @@ (dummy_produce (), dummy_stream ())
    in
    let t = root_hash h in
    (* Replay sequence of read/write in the same order, succeeds with stream entirely consumed *)
    let () =
      let ((t, s), value_a) = get (t, s) key_a in
      assert (value_a = value_x) ;
      let (t, s) = set (t, s) key_e value_y in
      let ((t, s), value_c) = get (t, s) key_c in
      assert (value_c = value_z) ;
      assert (Stream.Consumer.Raw.to_list s = []) ;
      ignore t
    in

    (* Starts with different root hash, fails *)
    let () =
      let h' =
        Bytes.sub (Bytes.cat (h) (Bytes.of_string "lol")) 2 32
      in
      let t = root_hash h' in
      assert_fail @@ fun () ->
      let _ = get (t, s) key_a in
      ()
    in

    (* Replay the sequence of read with any part of the stream altered, fails *)
    let () =
      let alter n s =
        Stream.Consumer.Raw.of_list
        @@ List.mapi
             (fun i (x : Stream.el) ->
               if i <> n then x
               else
                 match x with
                 | Empty -> Leaf (Bytes.of_string "lol")
                 | Leaf c -> Leaf (Bytes.cat c (Bytes.of_string "lol"))
                 | Node (left, right) ->
                     Node
                       ( THash.do_hash
                           (Bytes.cat
                              (left)
                              (Bytes.of_string "lol")),
                         right )
                 | _ -> raise Bad_stream)
             (Stream.Consumer.Raw.to_list s)
      in
      for i = 0 to List.length (Stream.Consumer.Raw.to_list s) - 1 do
        assert_fail @@ fun () ->
        let s = alter i s in
        let ((t, s), value_a) = get (t, s) key_a in
        assert (value_a = value_x) ;
        let (t, s) = set (t, s) key_e value_y in
        let ((t, s), value_c) = get (t, s) key_c in
        assert (value_c = value_z) ;
        let ((t, s), value_d) = get_opt (t, s) key_d in
        assert (value_d = None) ;
        assert (Stream.Consumer.Raw.to_list s = []) ;
        ignore t
      done
    in
    ()

  let tests =
    [
      test_get_raw;
      test_set_raw;
      test_set;
      test_mem;
      test_get;
      consume_store;
      nuke_tree;
    ]
end

module Extenders = struct
  exception Too_short

  let leaf x : Tree.t =
    Leaf {content = Bytes.of_string x; hash = THash.do_hash (Bytes.of_string x)}

  let node l r = Tree.Node {left = l; right = r; hash = Tree.lr_hash l r}

  let ext key node = Tree.Extender {node; hash = Tree.ext_hash node key; key}

  let rec take n key l =
    match key with
    | [] -> raise Too_short
    | h :: tl -> if n = 1 then not h :: l else take (n - 1) tl (h :: l)

  let string_to_key (s : string) =
    List.map
      (fun x -> if x = '0' then false else true)
      (List.of_seq @@ String.to_seq s)

  let ex s n = ext (string_to_key s) n

  let tree =
    let node1 = node (leaf "b") (leaf "c") in
    let ext1 = ex "1101" node1 in
    node ext1 (leaf "a")

  let tree2 =
    node
      (node
         (leaf "a")
         (ex
            "1010"
            (node
               (leaf "b")
               (ex "0100" (node (leaf "c") (node (leaf "d") (leaf "e")))))))
      (node (leaf "f") (ex "1100" (node (ex "1010" (leaf "g")) (leaf "h"))))

  let test_tree1 =
    test "Patricia_get" @@ fun () ->
    assert (snd @@ Patricia.get tree [true] = Bytes.of_string "a") ;
    assert (
      snd @@ Patricia.get tree (string_to_key "011011") = Bytes.of_string "c") ;
    assert (
      snd @@ Patricia.get tree (string_to_key "011010") = Bytes.of_string "b")

  let test_tree1_produce =
    test "Patricia_produce_get" @@ fun () ->
    let tree = (Patricia_produce_stream.of_tree tree, Stream.Producer.empty) in
    assert (snd @@ Patricia_produce_stream.get tree [true] = Bytes.of_string "a") ;
    assert (
      snd @@ Patricia_produce_stream.get tree (string_to_key "011011")
      = Bytes.of_string "c") ;
    assert (
      snd @@ Patricia_produce_stream.get tree (string_to_key "011010")
      = Bytes.of_string "b")

  let test_tree2 =
    test "Patricia_get_tree2" @@ fun () ->
    assert (snd @@ Patricia.get tree2 (string_to_key "00") = Bytes.of_string "a") ;
    assert (
      snd @@ Patricia.get tree2 (string_to_key "0110100") = Bytes.of_string "b") ;
    assert (
      snd @@ Patricia.get tree2 (string_to_key "011010101000")
      = Bytes.of_string "c") ;
    assert (
      snd @@ Patricia.get tree2 (string_to_key "0110101010010")
      = Bytes.of_string "d") ;
    assert (
      snd @@ Patricia.get tree2 (string_to_key "0110101010011")
      = Bytes.of_string "e") ;
    assert (snd @@ Patricia.get tree2 (string_to_key "10") = Bytes.of_string "f") ;
    assert (
      snd @@ Patricia.get tree2 (string_to_key "1111001") = Bytes.of_string "h") ;
    assert (
      snd @@ Patricia.get tree2 (string_to_key "11110001010")
      = Bytes.of_string "g")

  let test_tree2_produce =
    test "Patricia_get_produce_tree2" @@ fun () ->
    let tree2 =
      (Patricia_produce_stream.of_tree tree2, Stream.Producer.empty)
    in
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "00")
      = Bytes.of_string "a") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "0110100")
      = Bytes.of_string "b") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "011010101000")
      = Bytes.of_string "c") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "0110101010010")
      = Bytes.of_string "d") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "0110101010011")
      = Bytes.of_string "e") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "10")
      = Bytes.of_string "f") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "1111001")
      = Bytes.of_string "h") ;
    assert (
      snd @@ Patricia_produce_stream.get tree2 (string_to_key "11110001010")
      = Bytes.of_string "g")

  let tree3 =
    node
      (node (node (leaf "a") (leaf "b")) (node (leaf "c") (leaf "d")))
      (ex "011" (node (leaf "e") (leaf "f")))

  let suite_unit =
    [test_tree1; test_tree1_produce; test_tree2; test_tree2_produce]
end

let tests =
  [noop] @ Patricia_unit.tests @ Produce_unit.tests @ Consume_unit.tests
