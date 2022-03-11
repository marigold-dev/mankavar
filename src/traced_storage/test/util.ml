(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <gabriel.alfour@gmail.com>                    *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Tezos_traced_storage.Sparse

module THash = Tezos_traced_storage.Traced_hash

let test msg f = Alcotest.test_case msg `Quick f

let check condition msg = if condition then () else failwith msg

(* Used only in tests, dev, etc. *)
module Dev = struct
  let key_a = [true; true]

  let key_b = [false; false]

  let key_c = [false; true; false; false]

  let key_d = [false; true; false; false; true; true]

  let key_e = [true; false; true; false]

  let value_x = Bytes.of_string "foo"

  let value_y = Bytes.of_string "bar"

  let value_z = Bytes.of_string "42"

  let value_w = Bytes.of_string "wooo"
end

let noop = test "noop" @@ fun () -> ()

let assert_exn e f =
  try
    ignore @@ f () ;
    assert false
  with
  | ex when ex = e -> ()
  | _ -> assert false

let assert_invalid_key f = assert_exn Key.Invalid_key f

let assert_not_found f = assert_exn Not_found f

let assert_bad_stream f = assert_exn Patricia_consume_stream.Bad_stream f

let assert_fail f =
  try
    ignore @@ f () ;
    assert false
  with
  | Key.Invalid_key | Not_found | Patricia_consume_stream.Bad_stream -> ()
  | _ -> assert false

let assert_none f = assert (f () = None)

let make_key s =
  let aux = function '1' -> Some true | '0' -> Some false | _ -> None in
  List.filter_map aux @@ List.of_seq @@ String.to_seq s

let char_to_xint c =
  if 'a' <= c && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if '0' <= c && c <= '9' then Char.code c - Char.code '0' + 0
  else assert false

let hex_to_bytes x =
  let lst = List.of_seq @@ String.to_seq x in
  let rec aux acc = function
    | [] -> List.rev acc
    | a :: b :: tl ->
        let i = (16 * char_to_xint a) + char_to_xint b in
        let c = Char.chr i in
        aux (c :: acc) tl
    | _ -> assert false
  in
  let lst' = aux [] @@ List.tl @@ List.tl lst in
  Bytes.of_string @@ String.of_seq @@ List.to_seq lst'

let rec full_of_tree : Tree.t -> Patricia_consume_stream_impl.tree =
 fun t ->
  match t with
  | Empty -> Full Empty
  | Leaf {content; hash} -> Full (Leaf {content; hash})
  | Node {left; right; hash} ->
      let left = full_of_tree left in
      let right = full_of_tree right in
      Full (Node {left; right; hash})
  | Extender {key; node; hash} ->
      let node = full_of_tree node in
      Full (Extender {key; node; hash})

let dummy_patricia () : Patricia.tree =
  let open Patricia in
  let open Dev in
  let t = empty in
  let t = set t key_a value_x in
  let t = set t key_c value_z in
  t

let dummy_produce () : Patricia_produce_stream.tree =
  Patricia_produce_stream.of_tree @@ dummy_patricia ()

(* Example trees, for raw tests *)
(* raw_t1 =
        h
      /    \
    l_h    L z
   /  \
L x   lr_h
        | 0
       L y
*)
let raw_t1 : Tree.t =
  let open Tree in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = Leaf {content = ll_v; hash = ll_h} in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lrl = Leaf {content = lrl_v; hash = lrl_h} in
  let lr_h =
    THash.do_hash (Bytes.cat (lrl_h) (Key.key_to_bytes [false]))
  in
  let lr = Extender {node = lrl; key = [false]; hash = lr_h} in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = Node {left = ll; right = lr; hash = l_h} in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = Leaf {content = r_v; hash = r_h} in
  let h = THash.do_hash_pair l_h r_h in
  Node {left = l; right = r; hash = h}

let stream_t1_ll : Stream.el list =
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lr_h =
    THash.do_hash (Bytes.cat (lrl_h) (Key.key_to_bytes [false]))
  in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  [Leaf ll_v; Node (ll_h, lr_h); Node (l_h, r_h)]

let visited_t1_ll : Patricia_produce_stream_impl.tree =
  let open Patricia_produce_stream_impl.VTree in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = {tree = Leaf {content = ll_v; hash = ll_h}; visited = true} in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lrl = {tree = Leaf {content = lrl_v; hash = lrl_h}; visited = false} in
  let lr_h =
    THash.do_hash (Bytes.cat (lrl_h) (Key.key_to_bytes [false]))
  in
  let lr =
    {tree = Extender {node = lrl; key = [false]; hash = lr_h}; visited = false}
  in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = {tree = Node {left = ll; right = lr; hash = l_h}; visited = true} in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = {tree = Leaf {content = r_v; hash = r_h}; visited = false} in
  let h = THash.do_hash_pair l_h r_h in
  {tree = Node {left = l; right = r; hash = h}; visited = true}

let th_t1_ll : Patricia_consume_stream_impl.tree =
  let open Patricia_consume_stream_impl in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = Full (Leaf {content = ll_v; hash = ll_h}) in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lr_h =
    THash.do_hash (Bytes.cat (lrl_h) (Key.key_to_bytes [false]))
  in
  let lr = Hash lr_h in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = Full (Node {left = ll; right = lr; hash = l_h}) in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = Hash r_h in
  let h = THash.do_hash_pair l_h r_h in
  Full (Node {left = l; right = r; hash = h})

(* raw_t2 =
        h
      /    \
    l_h    L z
   /  \
L x   lr_h
      / \
  L y    L w
*)
let raw_t2 : Tree.t =
  let open Tree in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = Leaf {content = ll_v; hash = ll_h} in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lrl = Leaf {content = lrl_v; hash = lrl_h} in
  let lrr_v = Bytes.of_string "w" in
  let lrr_h = THash.do_hash lrr_v in
  let lrr = Leaf {content = lrr_v; hash = lrr_h} in
  let lr_h = THash.do_hash_pair lrl_h lrr_h in
  let lr = Node {left = lrl; right = lrr; hash = lr_h} in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = Node {left = ll; right = lr; hash = l_h} in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = Leaf {content = r_v; hash = r_h} in
  let h = THash.do_hash_pair l_h r_h in
  Node {left = l; right = r; hash = h}

let visited_raw_t2 : Patricia_produce_stream_impl.tree =
  let open Patricia_produce_stream_impl.VTree in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = {tree = Leaf {content = ll_v; hash = ll_h}; visited = false} in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lrl = {tree = Leaf {content = lrl_v; hash = lrl_h}; visited = false} in
  let lrr_v = Bytes.of_string "w" in
  let lrr_h = THash.do_hash lrr_v in
  let lrr = {tree = Leaf {content = lrr_v; hash = lrr_h}; visited = true} in
  let lr_h = THash.do_hash_pair lrl_h lrr_h in
  let lr =
    {tree = Node {left = lrl; right = lrr; hash = lr_h}; visited = true}
  in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = {tree = Node {left = ll; right = lr; hash = l_h}; visited = true} in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = {tree = Leaf {content = r_v; hash = r_h}; visited = false} in
  let h = THash.do_hash_pair l_h r_h in
  {tree = Node {left = l; right = r; hash = h}; visited = true}

let stream_t2_lrr : Stream.el list =
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lr_h =
    THash.do_hash (Bytes.cat (lrl_h) (Key.key_to_bytes [false]))
  in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  [Extender (lrl_h, [false]); Node (ll_h, lr_h); Node (l_h, r_h)]

let th_t2_lrr : Patricia_consume_stream_impl.tree =
  let open Patricia_consume_stream_impl in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = Hash ll_h in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lrl = Hash lrl_h in
  let lrr_v = Bytes.of_string "w" in
  let lrr_h = THash.do_hash lrr_v in
  let lrr = Full (Leaf {content = lrr_v; hash = lrr_h}) in
  let lr_h = THash.do_hash_pair lrl_h lrr_h in
  let lr = Full (Node {left = lrl; right = lrr; hash = lr_h}) in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = Full (Node {left = ll; right = lr; hash = l_h}) in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = Hash r_h in
  let h = THash.do_hash_pair l_h r_h in
  Full (Node {left = l; right = r; hash = h})

(* raw_t3 =
           h
         /    \
       l_h    L z
      /  \
   L x   lr_h
         / \
     L y   lrr_h
              | 1
             L w
*)
let raw_t3 : Tree.t =
  let open Tree in
  let ll_v = Bytes.of_string "x" in
  let ll_h = THash.do_hash ll_v in
  let ll = Leaf {content = ll_v; hash = ll_h} in
  let lrl_v = Bytes.of_string "y" in
  let lrl_h = THash.do_hash lrl_v in
  let lrl = Leaf {content = lrl_v; hash = lrl_h} in
  let lrrr_v = Bytes.of_string "w" in
  let lrrr_h = THash.do_hash lrrr_v in
  let lrrr = Leaf {content = lrrr_v; hash = lrrr_h} in
  let lrr_h =
    THash.do_hash (Bytes.cat (lrrr_h) (Key.key_to_bytes [true]))
  in
  let lrr = Extender {node = lrrr; key = [true]; hash = lrr_h} in
  let lr_h = THash.do_hash_pair lrl_h lrr_h in
  let lr = Node {left = lrl; right = lrr; hash = lr_h} in
  let l_h = THash.do_hash_pair ll_h lr_h in
  let l = Node {left = ll; right = lr; hash = l_h} in
  let r_v = Bytes.of_string "z" in
  let r_h = THash.do_hash r_v in
  let r = Leaf {content = r_v; hash = r_h} in
  let h = THash.do_hash_pair l_h r_h in
  Node {left = l; right = r; hash = h}
