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

open Das_helpers

type el =
  | Empty
  | Leaf of Bytes.t
  | Node of Hash'.t * Hash'.t
  | Extender of Hash'.t * Key.t
[@@deriving ez]

let el_encoding =
  let open Encoding in
  union [
    case get_empty_opt empty' unit ;
    case get_leaf_opt leaf bytes ;
    case get_node_opt node' @@ tuple_2 Hash'.encoding Hash'.encoding ;
    case get_extender_opt extender' @@ tuple_2 Hash'.encoding Key.encoding ;
  ]

module type CONSUMER = sig
  type t
  val empty : t
  val consume : t -> el option * t
  val encoding : t Encoding.t
end
module Consumer_raw = struct
  type t = el list
  let empty = []
  let consume s = match s with [] -> (None, s) | hd :: tl -> (Some hd, tl)
  let encoding = Encoding.(list el_encoding)
  let of_list x = x
  let to_list x = x
end

module type PRODUCER = sig
  type t
  type consumer
  val empty : t
  val to_consumer : t -> consumer
  val produce : t -> el -> t
  val encoding : t Encoding.t
end
module Producer_raw = struct
  type t = el list
  let empty = []
  let produce x y = y :: x
  let encoding = Encoding.(list el_encoding)
  let of_list x = x
  let to_list x = x
  let to_consumer s = Consumer_raw.of_list @@ List.rev (to_list s)
end

module rec Consumer : sig
  module Raw : module type of Consumer_raw
  include CONSUMER with type t = Raw.t
end = struct
  module Raw = Consumer_raw
  include Consumer_raw
end
and Producer : sig
  module Raw : module type of Producer_raw
  include PRODUCER with type t = Raw.t and type consumer := Consumer.t
end = struct
  module Raw = Producer_raw
  include Producer_raw
end