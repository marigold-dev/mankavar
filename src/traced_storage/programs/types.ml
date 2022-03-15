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

type value = bytes

type instruction = Get of Key.t | Set of (Key.t * value) | Mem of Key.t

type program = instruction list

type result = RGet of value option | RSet | RMem of bool

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
