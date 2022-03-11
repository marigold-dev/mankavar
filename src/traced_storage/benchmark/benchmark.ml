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
open Tezos_traced_storage.Stream
open Tezos_traced_storage_programs

let proof_size : Producer.t -> int =
 fun proof ->
  Bytes.length @@ Data_encoding.Binary.to_bytes_exn Producer.encoding proof

let proof_length : Producer.t -> int =
 fun s -> List.length (Producer.Dev.to_list s)

let get_key_len : instruction -> int = function
  | Get k | Mem k -> List.length k
  | Set (k, _) -> List.length k

let proof_max_key : program -> int =
 fun l ->
  let lengths = List.map get_key_len l in
  List.fold_left (fun l r -> max l r) 0 lengths

let safe_div n m = if m = 0 then 0 else n / m

let proof_avg_key : program -> int =
 fun l ->
  let lengths = List.map get_key_len l in
  let sum = List.fold_left (fun l r -> l + r) 0 lengths in
  let n_ops = List.length lengths in
  safe_div sum n_ops

let bench_program :
    (Patricia_produce_stream_impl.tree, program) Either.t ->
    program ->
    Producer.t =
 fun initial trial ->
  let init =
    match initial with Right setup -> setup_program setup | Left t -> t
  in
  let (_t, stream) =
    let open Patricia_produce_stream_impl in
    let aux : t -> instruction -> t =
     fun ts i ->
      match i with
      | Get k ->
          let (ts, _v) = get_opt ts k in
          ts
      | Set (k, v) -> set ts k v
      | Mem k ->
          let (ts, _v) = mem ts k in
          ts
    in
    List.fold_left aux (init, Producer.empty) trial
  in
  stream

type table_entry = {
  name : string;
  s_len : int;
  s_size : int;
  k_max : int;
  k_avg : int;
  trial_len : int;
  trial_avg : int;
}

let compute_table :
    (string * program * program * Producer.t) list -> table_entry list =
 fun progs ->
  List.map
    (fun (name, _, trial, stream) ->
      let s_len = proof_length stream in
      let s_size = proof_size stream in
      let k_max = proof_max_key trial in
      let k_avg = proof_avg_key trial in
      let trial_len = List.length trial in
      let trial_avg = safe_div s_size trial_len in
      {name; s_len; s_size; k_max; k_avg; trial_len; trial_avg})
    progs

let print_table table =
  let add_padding ?col_width s =
    match col_width with
    | None -> s ^ "  "
    | Some col_width ->
        s ^ String.init (col_width - String.length s) (fun _ -> ' ')
  in
  Format.(
    open_tbox () ;
    set_tab () ;
    printf "%s" (add_padding ~col_width:16 "Program") ;
    set_tab () ;
    printf "%s" (add_padding "Stream len") ;
    set_tab () ;
    printf "%s" (add_padding "Stream size (bytes)") ;
    set_tab () ;
    printf "%s" (add_padding "Max. key len") ;
    set_tab () ;
    printf "%s" (add_padding "Avg. key len") ;
    set_tab () ;
    printf "%s" (add_padding "Trial len") ;
    set_tab () ;
    printf "%s" (add_padding "Bytes / n_ops") ;
    printf "\n" ;
    List.iter
      (fun te ->
        print_tab () ;
        printf "%s" te.name ;
        print_tab () ;
        printf "%d" te.s_len ;
        print_tab () ;
        printf "%d" te.s_size ;
        print_tab () ;
        printf "%d" te.k_max ;
        print_tab () ;
        printf "%d" te.k_avg ;
        print_tab () ;
        printf "%d" te.trial_len ;
        print_tab () ;
        printf "%d" te.trial_avg)
      table ;
    close_tbox () ;
    printf "\n")

let () =
  Printf.printf "Benchmarks: \n" ;
  let programmed =
    List.filter_map
      (fun (name, setup, trial, valid) ->
        if valid then
          Some (name, setup, trial, bench_program (Right setup) trial)
        else None)
      programs
  in
  let generated =
    List.mapi
      (fun i (t, trial) ->
        ("rand_" ^ string_of_int i, [], trial, bench_program (Left t) trial))
      Generators.programs
  in
  let table = compute_table (programmed @ generated) in
  (* Dump generated examples *)
  (* let _ =
       let rec tree_of_produce : Patricia_produce_stream_impl.tree -> Tree.t =
        fun {tree; _} ->
         match tree with
         | Empty -> Empty
         | Leaf {content; hash} -> Leaf {content; hash}
         | Node {left; right; hash} ->
             Node
               {left = tree_of_produce left; right = tree_of_produce right; hash}
         | Extender {key; node; hash} ->
             Extender {key; node = tree_of_produce node; hash}
       in

       List.mapi
         (fun i (t, trial) ->
           Format.printf
             "%d:\nTree: %s \nTrial: %s\n\n"
             i
             (Generators.print_tree @@ tree_of_produce t)
             (String.concat ",\n" (List.map Generators.print_ins trial)))
         Generators.programs
     in *)
  (* Sorted by size/n_ops *)
  let table =
    List.sort (fun t1 t2 -> -1 * compare t1.trial_avg t2.trial_avg) table
  in
  print_table table
