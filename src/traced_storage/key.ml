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

(** A key k is said to be invalid in a tree t if:
    - There exists a strict prefix of k (i.e. a prefix different to k) that
      already has a value in t, or
    - k is a strict prefix of a key already present in t.
 *)
exception Invalid_key

type t = bool list
let encoding : t Encoding.t = Encoding.(list bool)
let compare = compare

let key_of_bits : bool list -> t = fun x -> x

let key_of_bytes : bytes -> t =
  let bits_of_bytes c =
    let i = Char.code c in
    let rec aux k t =
      if t = 0 then []
      else if k >= t then true :: aux (k - t) (t / 2)
      else false :: aux k (t / 2)
    in
    aux i 128
  in
  fun x -> List.concat_map bits_of_bytes @@ List.of_seq @@ Bytes.to_seq x

let key_to_string : t -> string =
 fun k -> String.concat "" @@ List.map (fun b -> if b then "1" else "0") k

let key_to_bytes : t -> bytes =
 fun k ->
  let byte_of_bits l =
    assert (List.length l = 8) ;
    let aux b = if b then 1 else 0 in
    List.fold_left (fun acc bit -> (2 * acc) + aux bit) 0 l
  in
  let add_padding k =
    let q = List.length k mod 8 in
    if q = 0 then k else List.append (List.init (8 - q) (fun _ -> false)) k
  in
  let rec bytes_of_bits k acc =
    match k with
    | [] -> List.rev acc
    | b0 :: b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: rest ->
        bytes_of_bits
          rest
          (Char.chr (byte_of_bits [b0; b1; b2; b3; b4; b5; b6; b7]) :: acc)
    | _ ->
        (* impossible: list length is a multiple of 8 *)
        assert false
  in
  Bytes.of_seq @@ List.to_seq @@ bytes_of_bits (add_padding k) []

let pp f lst =
  let open Format in
  List.iter (fun b -> fprintf f (if b then "1" else "0")) lst
