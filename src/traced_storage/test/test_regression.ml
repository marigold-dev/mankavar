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

let bug_regression_a =
  test "bug-regression-a" @@ fun () ->
  let stream =
    let open Patricia_produce_stream in
    let (t, s) =
      set (empty, Stream.Producer.empty) [true; false] (Bytes.of_string "w")
    in
    ignore t ;
    s
  in
  let () =
    let s = Stream.Producer.to_consumer stream in
    let h = Tree.get_hash Tree.empty in

    let open Patricia_consume_stream in
    let t = root_hash h in

    let (t, s) = set (t, s) [true; false] (Bytes.of_string "w") in
    assert (Stream.Consumer.Raw.to_list s = []) ;
    ignore t
  in

  ()

let tests = [bug_regression_a]
