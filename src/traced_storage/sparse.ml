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

module Key = Key
module Stream = Stream

open Traced_hash

let rec split_prefix l1 l2 acc =
  match (l1, l2) with
  | (_, []) | ([], _) -> (List.rev acc, l1, l2)
  | (h1 :: tl1, h2 :: tl2) ->
      if h1 = h2 then split_prefix tl1 tl2 (h1 :: acc)
      else (List.rev acc, l1, l2)

module Tree = struct
  (* Disable the warning 30: duplicate-definitions *)
  [@@@ocaml.warning "-30"]

  type node_content = {left : t; right : t; hash : Hash'.t}
  and extender_content = {node : t; hash : Hash'.t; key : Key.t}
  and leaf_content = {content:bytes;hash:Hash'.t}
  and t =
    | Node of node_content
    | Extender of extender_content
    | Leaf of leaf_content
    | Empty

  let empty = Empty

  let get_hash = function
    | Leaf {hash; _} | Node {hash; _} | Extender {hash; _} -> hash
    | Empty -> hash_empty

  let lr_hash l r = do_hash_pair (get_hash l) (get_hash r)

  let ext_hash node key =
    do_hash
      (Bytes.cat (get_hash node) (Key.key_to_bytes key))
end

module type COMMON = sig
  type t

  type nonrec key = Key.t

  type nonrec value = bytes

  (**
    The second component of [get_opt t k] is None if k is a valid key without a
    value in t.
    WARNING: This function can raise an exception. Notably, if k is not valid
    in t (see key.ml), then Invalid_key will be raised.
  *)
  val get_opt : t -> key -> t * value option

  val get : t -> key -> t * value

  val mem : t -> key -> t * bool

  (**
    WARNING: This function can raise an exception. Notably, if k is not valid
    in t (see key.ml), then Invalid_key will be raised.
  *)
  val set : t -> key -> value -> t

  val get_hash : t -> Hash'.t
end

module type PATRICIA = sig
  type tree = Tree.t

  include COMMON with type t = tree

  val empty : tree
end

module type PATRICIA_PRODUCE = sig
  type tree

  include COMMON with type t = tree * Stream.Producer.t

  val empty : tree

  val of_tree : Tree.t -> tree
end

module type PATRICIA_CONSUME = sig
  (**
    Exception raised by any of the operations of this module to reject a proof.
  *)
  exception Bad_stream

  type tree

  include COMMON with type t = tree * Stream.Consumer.t

  val empty : tree

  val root_hash : Hash'.t -> tree
end

module Patricia_impl = struct
  open Tree

  type nonrec t = t

  type nonrec key = Key.t

  type nonrec value = bytes

  type tree = t

  let empty = empty

  let rec get_aux t path =
    match (path, t) with
    | ([], Leaf {content; _}) -> Some content
    | ([], Node _) -> raise Key.Invalid_key
    | ([], Extender _) -> raise Key.Invalid_key
    | (_, Empty) -> None
    | (_ :: _, Extender {node; key; _}) -> (
        let (_, continue, diverge) = split_prefix key path [] in
        match (continue, diverge) with
        | (_ :: _, []) -> raise Key.Invalid_key
        | (_ :: _, _) -> None
        | _ -> get_aux node diverge)
    | (hd :: tl, Node {left; right; _}) -> (
        match hd with false -> get_aux left tl | true -> get_aux right tl)
    | (_ :: _, Leaf _) -> raise Key.Invalid_key

  let get_opt t path =
    let value = get_aux t path in
    (t, value)

  let mem t path =
    let value = match get_aux t path with Some _ -> true | None -> false in
    (t, value)

  let get t path =
    let value =
      match get_aux t path with Some x -> x | None -> raise Not_found
    in
    (t, value)

  type steps =
    | Left of t * steps
    | Right of t * steps
    | Extend of key * steps
    | Top

  type zipper = {steps : steps; tree : t}

  let rec rebuild : zipper -> t =
   fun {steps; tree} ->
    match steps with
    | Top -> tree
    | Left (right, steps) ->
        let left = tree in
        rebuild {steps; tree = Node {left; right; hash = lr_hash left right}}
    | Right (left, steps) ->
        let right = tree in
        rebuild {steps; tree = Node {left; right; hash = lr_hash left right}}
    | Extend (key, steps) ->
        let node = tree in
        rebuild {steps; tree = Extender {node; key; hash = ext_hash tree key}}

  let set t path value =
    let rec set_z : zipper -> key -> bytes -> zipper =
     fun z path value ->
      match (path, z.tree) with
      | ([], Leaf _) | ([], Empty) ->
          {z with tree = Leaf {content = value; hash = do_hash value}}
      | ([], Node _) | ([], Extender _) -> raise Key.Invalid_key
      | (hd :: tl, Node {left; right; _}) ->
          let steps =
            if hd then Right (left, z.steps) else Left (right, z.steps)
          in
          let z = {steps; tree = (if hd then right else left)} in
          set_z z tl value
      | (_ :: _, Extender {node; key; _}) -> (
          let (start, continue, diverge) = split_prefix key path [] in
          match (continue, diverge) with
          | (_ :: _, []) -> raise Key.Invalid_key
          | ([], _) -> (
              match node with
              | Empty ->
                  let steps = Extend (path, z.steps) in
                  let tree = Leaf {content = value; hash = do_hash value} in
                  {steps; tree}
              | _ ->
                  let steps = Extend (key, z.steps) in
                  let z = {steps; tree = node} in
                  set_z z diverge value)
          | (_ :: continue, h2 :: diverge) ->
              let leaf = Leaf {content = value; hash = do_hash value} in
              let divergent_node =
                if diverge = [] then leaf
                else
                  Extender
                    {key = diverge; node = leaf; hash = ext_hash leaf diverge}
              in
              let extend =
                if continue = [] then node
                else
                  Extender {node; key = continue; hash = ext_hash node continue}
              in
              let (left, right) =
                if h2 then (extend, divergent_node) else (divergent_node, extend)
              in
              let tree = Node {left; right; hash = lr_hash left right} in
              let steps =
                if start = [] then z.steps else Extend (start, z.steps)
              in
              {steps; tree})
      | (_ :: _, Empty) ->
          let steps = Extend (path, z.steps) in
          let tree = Leaf {content = value; hash = do_hash value} in
          {steps; tree}
      | (_ :: _, Leaf _) -> raise Key.Invalid_key
    in
    rebuild @@ set_z {tree = t; steps = Top} path value

  let get_hash tree =
    match tree with
    | Leaf {hash; _} | Node {hash; _} | Extender {hash; _} -> hash
    | Empty -> hash_empty
end

module Patricia : PATRICIA = Patricia_impl

module Patricia_produce_stream_impl = struct
  [@@@ocaml.warning "-30"]

  module VTree = struct
    type node_content = {left : t; right : t; hash : Hash'.t}

    and extender_content = {node : t; hash : Hash'.t; key : Key.t}

    and tree =
      | Node of node_content
      | Leaf of {content : bytes; hash : Hash'.t}
      | Extender of extender_content
      | Empty

    and t = {tree : tree; visited : bool}

    let rec of_tree : Tree.t -> t = function
      | Empty -> {tree = Empty; visited = false}
      | Leaf {content; hash} -> {tree = Leaf {content; hash}; visited = false}
      | Node {left; right; hash} ->
          let left = of_tree left in
          let right = of_tree right in
          {tree = Node {left; right; hash}; visited = false}
      | Extender {key; hash; node} ->
          {tree = Extender {key; hash; node = of_tree node}; visited = false}

    let get_hash {tree; _} =
      match tree with
      | Leaf {hash; _} | Node {hash; _} | Extender {hash; _} -> hash
      | Empty -> hash_empty

    let to_stream_el : t -> Stream.el =
     fun {tree; _} ->
      match tree with
      | Empty -> Empty
      | Leaf {content; _} -> Leaf content
      | Extender {node; key; _} ->
          let node_hash = get_hash node in
          Extender (node_hash, key)
      | Node {left; right; _} ->
          let left = get_hash left in
          let right = get_hash right in
          Node (left, right)

    let lr_hash l r = do_hash_pair (get_hash l) (get_hash r)

    let ext_hash node key =
      do_hash
        (Bytes.cat (get_hash node) (Key.key_to_bytes key))

    let pp_dump_tree =
      let rec aux n prefix path f t =
        let {tree; visited} = t in
        let open Format in
        for _ = 1 to n do
          fprintf f "| "
        done ;
        fprintf f "%s " prefix ;
        match tree with
        | Empty ->
            fprintf f "." ;
            if not visited then fprintf f " (X)"
        | Leaf {content; _} ->
            fprintf f "%a->%a" Key.pp (List.rev path) XBytes.pp_hex content ;
            if not visited then fprintf f " (X)" ;
            ()
        | Node {left; right; _} ->
            fprintf
              f
              "%s\n%a\n%a"
              (if not visited then "(X)" else "")
              (aux (n + 1) "L" (false :: path))
              left
              (aux (n + 1) "R" (true :: path))
              right
        | Extender {node; hash = _; key} ->
            fprintf
              f
              "%s%a %s\n%a"
              "Ex"
              Key.pp
              key
              (if not visited then "(X)" else "")
              (aux (n + 1) "N" (List.rev_append key path))
              node
      in
      aux 0 "Root" []

    let empty = {tree = Empty; visited = false}
  end

  open VTree

  type tree = VTree.t

  type nonrec t = tree * Stream.Producer.t

  type nonrec key = Key.t

  type nonrec value = bytes

  let of_tree = VTree.of_tree

  let empty = empty

  let produce s t = Stream.Producer.produce s (to_stream_el t)

  let node_hash left right = do_hash_pair (get_hash left) (get_hash right)

  let get_hash (t, _) = VTree.get_hash t

  type steps =
    | Left of VTree.t * steps
    | Right of VTree.t * steps
    | Extend of key * steps
    | Top

  type zipper = {steps : steps; t : VTree.t}

  let rec rebuild : zipper -> VTree.t =
   fun {steps; t} ->
    match steps with
    | Top -> t
    | Left (right, steps) ->
        let left = t in
        let t =
          {tree = Node {left; right; hash = lr_hash left right}; visited = true}
        in
        rebuild {steps; t}
    | Right (left, steps) ->
        let right = t in
        let t =
          {tree = Node {left; right; hash = lr_hash left right}; visited = true}
        in
        rebuild {steps; t}
    | Extend (key, steps) ->
        let node = t in
        let tree = Extender {node; key; hash = ext_hash t key} in
        rebuild {steps; t = {tree; visited = true}}

  let get_opt (t, stream) path =
    let rec get_opt_z :
        zipper * Stream.Producer.t ->
        key ->
        (zipper * Stream.Producer.t) * value option =
     fun (z, stream) path ->
      let {tree; visited} = z.t in
      match (path, tree) with
      | ([], Leaf {content; _}) ->
          let (t, stream) =
            if visited then (z.t, stream)
            else
              let stream = produce stream z.t in
              ({tree; visited = true}, stream)
          in
          let z = {z with t} in
          ((z, stream), Some content)
      | (hd :: tl, Node {left; right; _}) ->
          let stream = if visited then stream else produce stream z.t in
          let steps =
            if hd then Right (left, z.steps) else Left (right, z.steps)
          in
          let z = {steps; t = (if hd then right else left)} in
          get_opt_z (z, stream) tl
      | (_ :: _, Extender {node; key; _}) -> (
          let stream = if visited then stream else produce stream z.t in
          let (_, continue, diverge) = split_prefix key path [] in
          match (continue, diverge) with
          | (_ :: _, []) -> raise Key.Invalid_key
          | (_ :: _, _) ->
              let z = {z with t = {tree; visited = true}} in
              ((z, stream), None)
          | _ ->
              let steps = Extend (key, z.steps) in
              let z = {steps; t = node} in
              get_opt_z (z, stream) diverge)
      | (_ :: _, Leaf _) | ([], Node _) | ([], Extender _) ->
          raise Key.Invalid_key
      | (_, Empty) ->
          if visited then ((z, stream), None)
          else
            let stream = produce stream z.t in
            let z = {z with t = {tree; visited = true}} in
            ((z, stream), None)
    in
    let ((z, stream), o) = get_opt_z ({steps = Top; t}, stream) path in
    ((rebuild z, stream), o)

  let mem t path =
    match get_opt t path with
    | (ts, Some _) -> (ts, true)
    | (ts, None) -> (ts, false)

  let get t path =
    match get_opt t path with
    | (ts, Some x) -> (ts, x)
    | (_, None) -> raise Not_found

  (* One could have even shorter proofs by only persisting
     the hash of the branch not being modified instead of both *)
  let set (t, stream) path value =
    let rec set_z :
        zipper * Stream.Producer.t -> key -> value -> zipper * Stream.Producer.t
        =
     fun (z, stream) path value ->
      let {tree; visited} = z.t in
      match (path, tree) with
      | ([], Leaf _) | ([], Empty) ->
          let stream = if visited then stream else produce stream z.t in
          let t =
            {
              tree = Leaf {content = value; hash = do_hash value};
              visited = true;
            }
          in
          let z = {z with t} in
          (z, stream)
      | ([], Node _) | ([], Extender _) -> raise Key.Invalid_key
      | (hd :: tl, Node {left; right; _}) ->
          let stream = if visited then stream else produce stream z.t in
          let steps =
            if hd then Right (left, z.steps) else Left (right, z.steps)
          in
          let z = {steps; t = (if hd then right else left)} in
          set_z (z, stream) tl value
      | (_ :: _, Extender {key; node; _}) -> (
          let stream = if visited then stream else produce stream z.t in
          let (start, continue, diverge) = split_prefix key path [] in
          match (continue, diverge) with
          | (_ :: _, []) -> raise Key.Invalid_key
          | ([], _) -> (
              match node with
              | {tree = Empty; _} ->
                  let steps = Extend (path, z.steps) in
                  let t =
                    {
                      tree = Leaf {content = value; hash = do_hash value};
                      visited = true;
                    }
                  in
                  ({steps; t}, stream)
              | _ ->
                  let steps = Extend (key, z.steps) in
                  let z = {steps; t = node} in
                  set_z (z, stream) diverge value)
          | (_ :: continue, h :: diverge) ->
              let leaf =
                {
                  tree = Leaf {content = value; hash = do_hash value};
                  visited = true;
                }
              in
              let divergent_node =
                if diverge = [] then leaf
                else
                  {
                    tree =
                      Extender
                        {
                          node = leaf;
                          key = diverge;
                          hash = ext_hash leaf diverge;
                        };
                    visited = true;
                  }
              in
              let extend =
                if continue = [] then node
                else
                  {
                    tree =
                      Extender
                        {node; key = continue; hash = ext_hash node continue};
                    visited = true;
                  }
              in
              let (left, right) =
                if h then (extend, divergent_node) else (divergent_node, extend)
              in
              let t =
                {
                  tree = Node {left; right; hash = lr_hash left right};
                  visited = true;
                }
              in
              let steps =
                if start = [] then z.steps else Extend (start, z.steps)
              in
              ({steps; t}, stream))
      | (_ :: _, Empty) ->
          let stream = if visited then stream else produce stream z.t in
          let steps = Extend (path, z.steps) in
          let t =
            {
              tree = Leaf {content = value; hash = do_hash value};
              visited = true;
            }
          in
          ({steps; t}, stream)
      | (_ :: _, Leaf _) -> raise Key.Invalid_key
    in
    let (z, stream) = set_z ({steps = Top; t}, stream) path value in
    (rebuild z, stream)
end

module Patricia_produce_stream : PATRICIA_PRODUCE = Patricia_produce_stream_impl

module Patricia_consume_stream_impl = struct
  exception Bad_stream

  [@@@ocaml.warning "-30"]

  type tree = Hash of Hash'.t | Full of tree_view

  and node_content = {left : tree; right : tree; hash : Hash'.t}

  and leaf_content = {content : bytes; hash : Hash'.t}

  and extender_content = {node : tree; hash : Hash'.t; key : Key.t}

  and tree_view =
    | Leaf of leaf_content
    | Node of node_content
    | Extender of extender_content
    | Empty

  let get_tree_hash = function
    | Leaf {hash; _} | Node {hash; _} | Extender {hash; _} -> hash
    | Empty -> hash_empty

  let get_hash = function Hash x -> x | Full tr -> get_tree_hash tr

  type nonrec t = tree * Stream.Consumer.t

  type nonrec key = Key.t

  type nonrec value = bytes

  let empty = Full Empty

  let root_hash x = Hash x

  let do_leaf ~content ~hash =
    if do_hash content <> hash then raise Bad_stream else {content; hash}

  let node_hash left right = do_hash_pair (get_hash left) (get_hash right)

  let extender_hash node key =
    do_hash
      (Bytes.cat (get_hash node) (Key.key_to_bytes key))

  let do_node ~left ~right ~hash =
    if not (node_hash left right = hash) then raise Bad_stream ;
    {left; right; hash}

  let do_extender ~node ~key ~hash =
    if not (extender_hash node key = hash) then raise Bad_stream ;
    {node; key; hash}

  let get_hash (t, _) = get_hash t

  let of_stream_el : Stream.el -> Hash'.t -> tree_view =
   fun el hash ->
    match el with
    | Node (left, right) ->
        let node = do_node ~left:(Hash left) ~right:(Hash right) ~hash in
        Node node
    | Extender (node, key) ->
        let ext = do_extender ~node:(Hash node) ~key ~hash in
        Extender ext
    | Empty -> Empty
    | Leaf content ->
        let leaf = do_leaf ~content ~hash in
        Leaf leaf 

  let consume s t =
    match t with
    | Full t -> (t, s)
    | Hash h -> (
        let (o, s) = Stream.Consumer.consume s in
        match o with
        | Some el -> (of_stream_el el h, s)
        | None -> raise Bad_stream)

  type steps =
    | Left of tree * steps
    | Right of tree * steps
    | Extend of key * steps
    | Top

  type zipper = {steps : steps; t : tree}

  let rec rebuild : zipper -> tree =
   fun {steps; t} ->
    match steps with
    | Top -> t
    | Left (right, steps) ->
        let left = t in
        let t = Full (Node {left; right; hash = node_hash left right}) in
        rebuild {steps; t}
    | Right (left, steps) ->
        let right = t in
        let t = Full (Node {left; right; hash = node_hash left right}) in
        rebuild {steps; t}
    | Extend (key, steps) ->
        let node = t in
        let t = Full (Extender {node; key; hash = extender_hash t key}) in
        rebuild {steps; t}

  let get_opt (t, stream) path =
    let rec get_opt_z :
        zipper * Stream.Consumer.t ->
        key ->
        (zipper * Stream.Consumer.t) * value option =
     fun (z, stream) path ->
      match path with
      | [] -> (
          let (t, stream) = consume stream z.t in
          let z = {z with t = Full t} in
          match t with
          | Leaf {content; _} -> ((z, stream), Some content)
          | Node _ | Extender _ -> raise Key.Invalid_key
          | _ -> ((z, stream), None))
      | hd :: tl -> (
          let (tree, stream) = consume stream z.t in
          match tree with
          | Leaf _ -> raise Key.Invalid_key
          | Empty ->
              let z = {z with t = Full tree} in
              ((z, stream), None)
          | Node {left; right; _} ->
              let steps =
                if hd then Right (left, z.steps) else Left (right, z.steps)
              in
              let z = {steps; t = (if hd then right else left)} in
              get_opt_z (z, stream) tl
          | Extender {key; node; _} -> (
              let (_, continue, diverge) = split_prefix key path [] in
              let steps = Extend (key, z.steps) in
              let z = {steps; t = node} in
              match (continue, diverge) with
              | (_ :: _, []) -> raise Key.Invalid_key
              | (_ :: _, _) -> ((z, stream), None)
              | _ -> get_opt_z (z, stream) diverge))
    in

    let ((z, stream), o) = get_opt_z ({steps = Top; t}, stream) path in
    ((rebuild z, stream), o)

  let mem t path =
    match get_opt t path with
    | (ts, Some _) -> (ts, true)
    | (ts, None) -> (ts, false)

  let get t path =
    match get_opt t path with
    | (ts, Some x) -> (ts, x)
    | (_, None) -> raise Not_found

  let set (t, stream) path content =
    let rec set_z (z, stream) path content =
      match path with
      | [] -> (
          let (tree, stream) = consume stream z.t in
          match tree with
          | Node _ | Extender _ -> raise Key.Invalid_key
          | _ ->
              let leaf = do_leaf ~content ~hash:(do_hash content) in
              let z = {z with t = Full (Leaf leaf)} in
              (z, stream))
      | hd :: tl -> (
          let (tree, stream) = consume stream z.t in
          match tree with
          | Leaf _ -> raise Key.Invalid_key
          | Empty ->
              let steps = Extend (path, z.steps) in
              let t = Full (Leaf {content; hash = do_hash content}) in
              ({steps; t}, stream)
          | Node {left; right; _} ->
              let steps =
                if hd then Right (left, z.steps) else Left (right, z.steps)
              in
              let z = {steps; t = (if hd then right else left)} in
              set_z (z, stream) tl content
          | Extender {node; key; _} -> (
              let (start, continue, diverge) = split_prefix key path [] in
              match (continue, diverge) with
              | (_ :: _, []) -> raise Key.Invalid_key
              | ([], _) -> (
                  match node with
                  | Full Empty ->
                      let steps = Extend (path, z.steps) in
                      let t =
                        Full (Leaf {content; hash = do_hash content})
                      in
                      ({steps; t}, stream)
                  | _ ->
                      let steps = Extend (key, z.steps) in
                      let z = {steps; t = node} in
                      set_z (z, stream) diverge content)
              | (_ :: continue, h :: diverge) ->
                  let leaf =
                    Full (Leaf {content; hash = do_hash content})
                  in
                  let divergent_node =
                    if diverge = [] then leaf
                    else
                      Full
                        (Extender
                           {
                             key = diverge;
                             node = leaf;
                             hash = extender_hash leaf diverge;
                           })
                  in
                  let extend =
                    if continue = [] then node
                    else
                      Full
                        (Extender
                           {
                             node;
                             key = continue;
                             hash = extender_hash node continue;
                           })
                  in
                  let (left, right) =
                    if h then (extend, divergent_node)
                    else (divergent_node, extend)
                  in
                  let t =
                    Full (Node {left; right; hash = node_hash left right})
                  in
                  let steps =
                    if start = [] then z.steps else Extend (start, z.steps)
                  in
                  ({steps; t}, stream)))
    in
    let (z, stream) = set_z ({steps = Top; t}, stream) path content in
    (rebuild z, stream)
end

module Patricia_consume_stream : PATRICIA_CONSUME = Patricia_consume_stream_impl
