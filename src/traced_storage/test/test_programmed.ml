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

open Util
open Traced_storage.Sparse
open Traced_storage_programs

(* let print_trace stream =
  Format.printf "Trace\n" ;
  List.iter (Format.printf "%a\n" hex) (Stream.Producer.Raw.to_bytes stream) ;
  () *)

let i = ref 0

let check_i n =
  i := !i + 1 ;
  n = !i

let test_program : program -> program -> unit =
 fun setup trial ->
  let init = setup_program setup in
  (* Format.printf "Pre State:\n%a\n\n" pp_dump_tree init ; *)
  let ((_t, stream), vs) =
    let open Patricia_produce_stream_impl in
    let aux : t * result list -> instruction -> t * result list =
     fun (ts, vs) i ->
      (* Format.printf "Process instruction\n" ; *)
      match i with
      | Get k ->
          (* Format.printf "Get\n" ;
           * print_trace (snd ts) ;
           * if check_i 1 then Format.printf "State:\n%a\n" pp_dump_tree (fst ts) ; *)
          let (ts, v) = get_opt ts k in
          (* print_trace (snd ts) ;
           * if check_i 2 then Format.printf "State:\n%a\n" pp_dump_tree (fst ts) ; *)
          (ts, RGet v :: vs)
      | Set (k, v) ->
          (* Format.printf "Set\n" ; *)
          let ts = set ts k v in
          (ts, RSet :: vs)
      | Mem k ->
          (* Format.printf "Mem\n" ; *)
          let (ts, v) = mem ts k in
          (ts, RMem v :: vs)
    in
    List.fold_left aux ((init, Stream.Producer.empty), []) trial
  in

  (* Format.printf "Post State:\n%a\n\n" pp_dump_tree _t ; *)
  (* print_trace stream ; *)
  (* (
   *   match _t with
   *   | Node nc -> assert (not nc.visited)
   *   | _ -> assert false
   * ) ; *)
  let rec aux :
      program ->
      Patricia_consume_stream.tree ->
      Stream.Consumer.t ->
      result list ->
      _ =
   fun p t s vs ->
    let open Patricia_consume_stream in
    match p with
    | [] ->
        if Stream.Consumer.Raw.to_list s <> [] then
          failwith "state trace not empty at the end" ;
        if vs <> [] then failwith "some values left at the end" ;
        (vs, s)
    | hd :: tl -> (
        let (v_hd, v_tl) =
          match vs with [] -> failwith "no values left" | hd :: tl -> (hd, tl)
        in
        match hd with
        | Get k ->
            let ((t, s), v) = get_opt (t, s) k in
            assert (v_hd = RGet v) ;
            aux tl t s v_tl
        | Mem k ->
            let ((t, s), v) = mem (t, s) k in
            assert (v_hd = RMem v) ;
            aux tl t s v_tl
        | Set (k, v) ->
            let (t, s) = set (t, s) k v in
            assert (v_hd = RSet) ;
            aux tl t s v_tl)
  in
  let (vs, s) =
    aux
      trial
      (Patricia_consume_stream.root_hash
         (Patricia_produce_stream_impl.get_hash (init, stream)))
      (Stream.Producer.to_consumer stream)
      (List.rev vs)
  in
  assert (vs = []) ;
  assert (Stream.Consumer.Raw.to_list s = []) ;
  ()

let test_hex =
  test "hex" @@ fun () ->
  let str = "woood" in
  let bytes = Bytes.of_string str in
  let x = Format.asprintf "%a" Das_helpers.XBytes.pp_hex bytes in
  assert (hex_to_bytes x = bytes) ;
  (* Format.printf "%s -> %s" str x ; *)
  ()

let make_test (name, setup, trial, valid) =
  test name @@ fun () ->
  if valid then test_program setup trial
  else assert_fail (fun () -> test_program setup trial)

let tests = [test_hex] @ List.map make_test programs
