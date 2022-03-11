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
open Tezos_traced_storage_programs
open Tezos_traced_storage_programs.Generators

module Common_tests
    (S : COMMON) (G : sig
      val tp_gen : (S.t * Key.t) QCheck.arbitrary

      val name : string
    end) =
struct
  let count = 1000

  let safe_set t p v =
    try
      let t = S.set t p v in
      Some t
    with _ -> None

  let of_some x = match x with Some x -> x | _ -> assert false

  let get_set (t, p) =
    let v = Bytes.of_string "x" in
    let t = safe_set t p v in
    QCheck.assume (Option.is_some t) ;
    let t = of_some t in
    let (_, r) = S.get_opt t p in
    r = Some v

  let test_get_set =
    QCheck.Test.make
      ~name:(G.name ^ ": G k (S k v) = v")
      ~count
      G.tp_gen
      get_set

  let set_set (t, p) =
    let v1 = Bytes.of_string "x" in
    let v2 = Bytes.of_string "y" in
    let t1 = safe_set t p v1 in
    QCheck.assume (Option.is_some t1) ;
    let t1 = of_some t1 in
    let t2 = safe_set t1 p v2 in
    let t2 = of_some t2 in
    let t2' = safe_set t p v2 in
    let t2' = of_some t2' in
    t2 = t2'

  let test_set_set =
    QCheck.Test.make
      ~name:(G.name ^ ": S k v2 (S k v1) = S k v2")
      ~count
      G.tp_gen
      set_set

  let get_get (t, p) =
    let (t1, r1) = S.get_opt t p in
    let (t2, r2) = S.get_opt t1 p in
    (t2, r2) = (t1, r1)

  let test_get_get =
    QCheck.Test.make
      ~name:(G.name ^ ": G k (G k) = G k")
      ~count
      G.tp_gen
      get_get

  let set_get (t, p) =
    let v = Bytes.of_string "x" in
    let (t1, _) = S.get_opt t p in
    let t2 = safe_set t1 p v in
    QCheck.assume (Option.is_some t2) ;
    let t2 = of_some t2 in
    let t2' = safe_set t p v in
    let t2' = of_some t2' in
    t2 = t2'

  let test_set_get =
    QCheck.Test.make
      ~name:(G.name ^ ": S k v (G k) = S k v")
      ~count
      G.tp_gen
      set_get

  let alg_props (t, p) =
    List.fold_left ( && ) true
    @@ List.map (fun f -> f (t, p)) [get_set; set_set; get_get; set_get]

  let test_algebraic =
    QCheck.Test.make
      ~name:(G.name ^ ": algebraic properties")
      ~count
      G.tp_gen
      alg_props

  let tests = [test_algebraic]
  (* [test_get_set; test_set_set; test_get_get; test_set_get] *)
end

module Patricia_tests =
  Common_tests
    (Patricia)
    (struct
      let tp_gen = QCheck.make ~print:print_tree_path tree_path_gen

      let name = "Patricia"
    end)

module Produce_tests =
  Common_tests
    (Patricia_produce_stream_impl)
    (struct
      let tp_gen =
        let gen =
          QCheck.Gen.(
            let* (t, p) = tree_path_gen in
            let tree = Patricia_produce_stream_impl.VTree.of_tree t in
            let t = (tree, Stream.Producer.empty) in
            return (t, p))
        in
        QCheck.make gen

      let name = "Produce"
    end)

module Consume_tests =
  Common_tests
    (Patricia_consume_stream_impl)
    (struct
      let tp_gen =
        let gen =
          QCheck.Gen.(
            let* (t, p) = tree_path_gen in
            let tree = Util.full_of_tree t in
            let t = (tree, Stream.Consumer.empty) in
            return (t, p))
        in
        QCheck.make gen

      let name = "Consume"
    end)

module Programmed = struct
  let count = 1000

  let check_program : Patricia_produce_stream_impl.tree * program -> bool =
   fun (init, trial) ->
    let ((_t, stream), vs) =
      let open Patricia_produce_stream_impl in
      let safe_set t p v =
        try
          let t = set t p v in
          Some t
        with _ -> None
      in
      let of_some x = match x with Some x -> x | _ -> assert false in
      let apply_produce : t * result list -> instruction -> t * result list =
       fun (ts, vs) i ->
        match i with
        | Get k ->
            let (ts, v) = get_opt ts k in
            (ts, RGet v :: vs)
        | Set (k, v) ->
            let ts = safe_set ts k v in
            QCheck.assume (Option.is_some ts) ;
            (of_some ts, RSet :: vs)
        | Mem k ->
            let (ts, v) = mem ts k in
            (ts, RMem v :: vs)
      in
      List.fold_left apply_produce ((init, Stream.Producer.empty), []) trial
    in

    let rec apply_consume :
        program ->
        Patricia_consume_stream_impl.tree ->
        Stream.Consumer.t ->
        result list ->
        _ =
     fun p t s vs ->
      let open Patricia_consume_stream_impl in
      let safe_set t p v =
        try
          let t = set t p v in
          Some t
        with _ -> None
      in
      let of_some x = match x with Some x -> x | _ -> assert false in
      match p with
      | [] -> (vs, s)
      | hd :: tl -> (
          let (v_hd, v_tl) =
            match vs with
            | [] -> failwith "no values left"
            | hd :: tl -> (hd, tl)
          in
          match hd with
          | Get k ->
              let ((t, s), v) = get_opt (t, s) k in
              assert (v_hd = RGet v) ;
              apply_consume tl t s v_tl
          | Mem k ->
              let ((t, s), v) = mem (t, s) k in
              assert (v_hd = RMem v) ;
              apply_consume tl t s v_tl
          | Set (k, v) ->
              let ts = safe_set (t, s) k v in
              QCheck.assume (Option.is_some ts) ;
              let (t, s) = of_some ts in
              assert (v_hd = RSet) ;
              apply_consume tl t s v_tl)
    in
    try
      let (vs, s) =
        apply_consume
          trial
          (Patricia_consume_stream_impl.root_hash
             (Patricia_produce_stream_impl.get_hash (init, stream)))
          (Stream.Producer.to_consumer stream)
          (List.rev vs)
      in
      vs = [] && Stream.Consumer.Raw.to_list s = []
    with Patricia_consume_stream.Bad_stream -> false

  let test_program =
    QCheck.Test.make
      ~name:"Prover accepts proof"
      ~count
      (QCheck.make ~print:tree_program_print tree_program_gen)
      check_program

  let tests = [test_program]
end

let tests =
  List.map
    QCheck_alcotest.to_alcotest
    (Patricia_tests.tests @ Produce_tests.tests @ Consume_tests.tests
   @ Programmed.tests)
