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

open Traced_storage.Sparse
open Types

let leaf content : Tree.t = Leaf {content; hash = Traced_storage.Traced_hash.do_hash content}

let node l r : Tree.t = Node {left = l; right = r; hash = Tree.lr_hash l r}

(* We fix the size of the leaves contents *)
let leaf_size_gen = QCheck.Gen.return 100

let leaf_content_gen =
  QCheck.Gen.(map Bytes.of_string (string_size leaf_size_gen))

let leaf_gen = QCheck.Gen.(map leaf leaf_content_gen)

let ext key node = Tree.Extender {node; hash = Tree.ext_hash node key; key}

(* Generators *)

let tree_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
           match n with
           | 0 -> leaf_gen
           | _ -> (
               match n mod 4 with
               | 2 -> map2 ext (list_size (int_range 1 3) bool) (self (n / 2))
               | 1 | 3 -> map2 node (self (n / 2)) (self (n / 3))
               | _ -> self (n / 4))))

let rec print_tree = function
  | Tree.Leaf _ -> "L"
  | Tree.Empty -> "E"
  | Tree.Node {left; right; _} ->
      "N(" ^ print_tree left ^ ", " ^ print_tree right ^ ")"
  | Tree.Extender {key; node; _} ->
      "Ext(" ^ Key.key_to_string key ^ ", " ^ print_tree node ^ ")"

let sub_key k =
  let rec take l n =
    match (n, l) with
    | (_, []) -> []
    | (0, _) -> []
    | (n, x :: xs) -> x :: take xs (n - 1)
  in
  QCheck.Gen.(take k <$> int_bound (List.length k))

let path_gen_derived t =
  QCheck.Gen.(
    let rec go (t : Tree.t) path =
      match t with
      | Leaf _ -> return (List.rev path)
      | Empty ->
          let* bs = list bool in
          return (List.rev_append path bs)
      | Node {left; right; _} ->
          let* b = bool in
          if b then go right (b :: path) else go left (b :: path)
      | Extender {key; node; _} ->
          frequency
            [
              (1, go node (List.rev_append key path));
              (* path doesn't go through [node] *)
              ( 1,
                let rec same_prefix l1 l2 =
                  match (l1, l2) with
                  | ([], []) -> true
                  | ([], _) -> true
                  | (_, []) -> true
                  | (x :: xs, y :: ys) -> x = y && same_prefix xs ys
                in
                let rec new_key key =
                  let* piece_of_key = sub_key key in
                  let* cont = list_size (int_range 1 20) bool in
                  let nkey = piece_of_key @ cont in
                  if same_prefix nkey key then new_key key else return nkey
                in
                let* nkey = new_key key in
                return @@ List.rev_append path nkey );
            ]
    in
    go t [])

let path_gen_random = QCheck.Gen.(list bool)

let path_gen t = path_gen_derived t

let tree_path_gen =
  QCheck.Gen.(
    let* t = tree_gen in
    let* p = path_gen t in
    return (t, p))

let print_tree_path (t, p) =
  "T: " ^ print_tree t ^ "\n P: " ^ Key.key_to_string p

let max_prog_length = 20

let instruction_gen t =
  QCheck.Gen.(
    frequency
      [
        (1, path_gen t >>= fun k -> return @@ Get k);
        (1, path_gen t >>= fun k -> return @@ Mem k);
        ( 1,
          path_gen t >>= fun k ->
          leaf_content_gen >>= fun s -> return (Set (k, s)) );
      ])

let print_ins = function
  | Get k -> "G " ^ Key.key_to_string k
  | Mem k -> "M " ^ Key.key_to_string k
  | Set (k, _) -> "S " ^ Key.key_to_string k

let program_gen t =
  (* We want to only generate valid traces (i.e. no Invalid_key) *)
  QCheck.Gen.(
    let program_gen_n =
      fix (fun self (t, n, previous) ->
          match n with
          | 0 -> return @@ List.rev previous
          | _ ->
              let* ins = instruction_gen t in
              let k =
                match ins with Set (k, _) -> k | Mem k -> k | Get k -> k
              in
              let sets_keys =
                List.filter_map
                  (function Set (k, _) -> Some k | _ -> None)
                  previous
              in
              let rec same_prefix_strict l1 l2 =
                match (l1, l2) with
                | ([], []) -> false
                | ([], _) -> true
                | (_, []) -> true
                | (x :: xs, y :: ys) -> x = y && same_prefix_strict xs ys
              in
              let valid_k =
                List.for_all (fun k' -> not (same_prefix_strict k k')) sets_keys
              in
              if valid_k then self (t, n - 1, ins :: previous)
              else self (t, n, previous))
    in

    let* n = int_bound max_prog_length in
    program_gen_n (t, n, []))

let tree_program_gen =
  QCheck.Gen.(
    let* t = tree_gen in
    let tree = Patricia_produce_stream_impl.VTree.of_tree t in
    let* prog = program_gen t in
    return (tree, prog))

let tree_program_print :
    Patricia_produce_stream_impl.tree * instruction list -> string =
 fun (t, prog) ->
  let rec tree_of_produce : Patricia_produce_stream_impl.tree -> Tree.t =
   fun tr ->
    match tr.tree with
    | Empty -> Empty
    | Leaf {content; hash} -> Leaf {content; hash}
    | Node {left; right; hash} ->
        Node {left = tree_of_produce left; right = tree_of_produce right; hash}
    | Extender {node; hash; key} ->
        Extender {node = tree_of_produce node; hash; key}
  in
  "T: "
  ^ print_tree (tree_of_produce t)
  ^ "\n P: "
  ^ String.concat ", " (List.map print_ins prog)

(* Set a seed for reproducibility *)
let programs =
  QCheck.Gen.generate ~rand:(Random.State.make [|1|]) ~n:100 tree_program_gen
