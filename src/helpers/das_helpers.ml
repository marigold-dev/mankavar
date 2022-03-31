module Crypto = Crypto
module PseudoEffect = PseudoEffect
module Encoding = Encoding
module XOption = XOption
module Linear_state = Linear_state
module Bimap = Bimap
module XMap = XMap
module Bits = Bits

let tuple2 x y = (x , y)
let tuple3 x y z = (x , y, z)

module XBool = struct
  let do_if_true b f = if b then f ()
end

module XResult = struct
  let value' on_error = function
  | Ok x -> x
  | Error e -> on_error e
end
module XArray = struct
  let empty () = Array.make 0 (Obj.magic ()) 
end
module XChar = struct
  let map_int f c =
    let i = Char.code c in
    Char.chr ((f i) mod 256)
end
module XUnit = struct
  type t = unit
  let encoding = Encoding.unit
  let of_bytes = Encoding.of_bytes encoding
end
module XInt64 = struct
  include Int64
  let compare = Int64.compare
  let encoding = Encoding.int64
  let to_bits = Encoding.to_bits encoding
  let pp' = Format.dprintf "%Ld"
  let pp = Fun.flip pp'
  let (<) (a : Int64.t) b = a < b
  let (<=) (a : Int64.t) b = a <= b
  let (>) (a : Int64.t) b = a > b
end

module XBytes = struct
  let pp_char ppf c = Format.fprintf ppf "%02x" @@ Char.code c
  let pp ppf b =
    Bytes.iter (pp_char ppf) b
  let pp_hex ppf b =
    Format.fprintf ppf "0x%a" pp b
end


module XFormat = XFormat

module Hash : sig
  type 'a t
  val make : ('a -> bytes) -> 'a -> 'a t
  val to_bytes : 'a t -> bytes
  val dummy : 'a t
  val encoding : 'a t Encoding.t
  val compare : 'a t -> 'a t -> int
  val pp : 'any -> Format.formatter -> 'a t -> unit
  val pp' : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = bytes
  let make f x = f x
  let to_bytes x = x
  let dummy = Bytes.empty
  let encoding = Encoding.bytes
  let pp' = XBytes.pp
  let pp = fun _ -> pp'
  let compare = Bytes.compare
end
type 'a hash = 'a Hash.t

module Hash' = struct
  type t = bytes
  let dummy = Bytes.empty
  let encoding = Encoding.bytes
  let pp = XBytes.pp
  let compare = Bytes.compare
end
type hash' = Hash'.t

module XPtime = struct
  open Ptime

  let dd = 86_400_000_000_000_000L

  let span_map_d_ps : _ -> Ptime.Span.t -> Ptime.Span.t = fun f s ->
    let (d , ps) = s |> Span.to_d_ps in
    let (d' , ps') = f (d , ps) in
    Span.of_d_ps (d' , ps') |> Option.get

  let map_d_ps : _ -> Ptime.t -> Ptime.t = fun f s ->
    s |> to_span |> span_map_d_ps f |> of_span |> Option.get


  let ms_to_ps = 1_000_000_000L
  let truncate_ms : Ptime.t -> int -> Ptime.t = fun s i ->
    if Int64.(rem dd (mul ms_to_ps (of_int i)) <> zero)
       then failwith "not compatible truncation" ;
    s |> map_d_ps @@ fun (d , ps) ->
      let ps' =
      let open Int64 in
      sub ps
      @@ rem ps 
        @@ mul ms_to_ps @@ of_int i
    in
    (d , ps')

  let ms_int : int -> Ptime.Span.t = fun i ->
    (0 , Int64.(mul ms_to_ps @@ of_int i))
    |> Span.of_d_ps |> Option.get

  let span_mul_int : Ptime.Span.t -> int -> Ptime.Span.t = fun s i ->
    s |> span_map_d_ps @@ fun (d , ps) ->
    let mul_d = d * i in
    let mul_ps = Int64.(mul ps @@ of_int i) in
    let mul_ps_d = Int64.(div mul_ps dd) in
    let d' = mul_d + (Int64.to_int mul_ps_d) in
    let ps' = Int64.(rem mul_ps dd) in
    (d' , ps')

  let now : unit -> Ptime.t = fun () ->
    Unix.gettimeofday () |> of_float_s |> Option.get

  let span_encoding : span Encoding.t = Encoding.(
    conv (fun x -> Span.of_d_ps x |> Option.get) (Span.to_d_ps) @@
    tuple_2 int int64
  )
  let encoding : t Encoding.t = Encoding.(
    conv (fun x -> Ptime.of_span x |> Option.get) (Ptime.to_span) span_encoding
  )

  let pp_ms = pp_human ~frac_s:3 ()
end

module XList = struct
  (* List.length lst >= 1 *)
  let rec last lst =
    match lst with
    | [] -> assert false
    | [ x ] -> x
    | _ :: tl -> last tl

  let is_sorted cmp =
    let rec aux lst =
      match lst with
      | [] -> true
      | [ _ ] -> true
      | x :: y :: tl -> cmp x y >= 0 && aux (y :: tl)
    in
    aux

  let nth_map f n lst =
    let rec aux acc i lst = match lst with
    | [] -> assert false
    | hd :: tl when i = 0 -> (List.rev acc) @ (f hd :: tl)
    | hd :: tl -> aux (hd :: acc) (i - 1) tl
    in
    aux [] n lst

  (*
    Find following elements matching a predicate
    find_follow (fun a b -> b - a = 4) [ 1 ; 2 ; 4 ; 8 ; 16 ] = Some (4 , 8)
    find_follow (fun a b -> b - a = 3) [ 1 ; 2 ; 4 ; 8 ; 16 ] = None
  *)
  let find_follow f =
    let rec aux lst =
      match lst with
      | [] -> None
      | [ _ ] -> None
      | x :: y :: tl -> if f x y then Some (x , y) else aux (y :: tl)
    in
    aux

  (*
    Check if all following elements match a predicate
    foralli_follow (fun i a b -> a + i = b) [ 0 ; 0 ; 1 ; 3 ; 6 ; 10] = true
    foralli_follow (fun i a b -> a + i = b) [ 0 ; 0 ; 1 ; 3 ; 4 ; 9] = false
  *)
  let foralli_follow f =
    let rec aux i lst =
      match lst with
      | [] -> true
      | [ _ ] -> true
      | x :: y :: tl -> if f i x y then aux (i + 1) (y :: tl) else false
    in
    aux 0

  (*
    Group `lst` in sub lists of `n` elements each.
    - last sub list is allowed to have less than `n` elements.
    - no empty sub lists
    - preserves order
  *)
  let group_by n lst =
    let rec aux i acc_sub acc_all lst = match lst with
    | [] -> List.rev ((List.rev acc_sub) :: acc_all)
    | hd :: tl when i mod n = 0 ->
      aux 1 [] ((List.rev (hd :: acc_sub)) :: acc_all) tl
    | hd :: tl ->
      aux (i + 1) (hd :: acc_sub) acc_all tl
    in
    aux 1 [] [] lst

  (*
    Grou `lst` in sub lists.
    - no empty sub lists
    - preserves order
    - `f` states if two consecutive elements should be in same sub list
  *)
  let group_by_f f lst =
    let rec aux acc_all acc_current prec_opt lst =
      let flush () =
        if acc_current = [] then acc_all else
        (List.rev acc_current) :: acc_all
      in
      match lst , prec_opt with
      | [] , _ -> flush () |> List.rev
      | hd :: tl , Some prec when f hd prec ->
        aux acc_all (hd :: acc_current) (Some hd) tl
      | hd :: tl , None ->
        aux acc_all (hd :: acc_current) (Some hd) tl
      | hd :: tl , _ ->
        aux ((List.rev acc_current) :: acc_all) [hd] (Some hd) tl
    in
    if lst = [] then [] else
    aux [] [] None lst
end

module Index = struct
  module Raw = struct
    type t =
    | Height of int32
    [@@deriving ez]

    let of_int32 = height
    let to_int32 = get_height_exn
    let to_int x = x |> to_int32 |> Int32.to_int
    let of_int x = x |> Int32.of_int |> of_int32
    let map_int32 f x = of_int32 @@ f @@ to_int32 x
    let map_int f x = of_int @@ f @@ to_int x
    let compare : t -> t -> int = fun x y ->
      Int32.compare (to_int32 x) (to_int32 y)
    let equal a b = compare a b = 0
    let zero = height Int32.zero
    let increment t =
      t
      |> map_height Int32.succ
    let predecessor t = t |> map_height Int32.pred
    let encoding = Encoding.(conv of_int32 to_int32 int32)
    module Map = struct
      let encoding' = encoding
      include XMap.Make(struct type nonrec t = t let compare = compare end)
      let encoding x = encoding encoding' x
    end
    let pp ppf x = Format.fprintf ppf "%ld" (x |> get_height_exn)
    let (<) a b = compare a b < 0
    let (<=) a b = compare a b <= 0
    let (>) a b = compare a b > 0
    let (>=) a b = compare a b >= 0
    let (<>) a b = compare a b <> 0
    let (=) a b = compare a b = 0
  end

  module Make() : sig
    type t
    val of_int32 : int32 -> t
    val to_int32 : t -> int32
    val to_int : t -> int
    val of_int : int -> t
    val map_int32 : (int32 -> int32) -> t -> t
    val map_int : (int -> int) -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val zero : t
    val increment : t -> t
    val predecessor : t -> t
    val pp : Format.formatter -> t -> unit
    val encoding : t Encoding.t
    module Map : sig
      include module type of XMap.Make(
        struct type nonrec t = t let compare = compare end
      )
      val encoding : 'a Encoding.t -> 'a t Encoding.t
    end
    val (<) : t -> t -> bool
    val (<=) : t -> t -> bool
    val (>) : t -> t -> bool
    val (>=) : t -> t -> bool
    val (<>) : t -> t -> bool
    val (=) : t -> t -> bool
  end = struct include Raw end
end

module Height = Index.Make()

module MapList = struct
  module M = XMap.Make(Int)
  module Raw = struct
    type 'a t = {
      size : int ;
      content : 'a M.t ;
    }
    [@@deriving ez]
    let empty = make_tpl 0 M.empty
    let get_opt i t = M.find_opt i t.content
    let get i t = M.find i t.content
    let append v t =
      make_tpl (t.size + 1) (M.add t.size v t.content)
    let remove i t =
      map_content (M.remove i) t
    let fold f t = M.fold f t.content
    let first_opt t =
      let s = size t in
      let rec aux i =
        if i >= s then None
        else match get_opt i t with
        | None -> aux (i + 1)
        | Some x -> Some x
      in
      aux 0
    let is_empty t = Option.is_none @@ first_opt t
    let encoding e =
      let open Encoding in
      conv make_tpl' destruct @@ tuple_2 int @@ M.encoding int e
  end
  module type SIGNATURE = sig
    (*
      MapList is ever growing.
      Even if you remove an element, size does not decrease.
      Practical so that indexes are kept.  
    *)
    type 'a t
    val empty : 'a t
    val get_opt : int -> 'a t -> 'a option
    val get : int -> 'a t -> 'a
    val append : 'a -> 'a t -> 'a t
    val remove : int -> 'a t -> 'a t
    val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val size : 'a t -> int
    val first_opt : 'a t -> 'a option
    val is_empty : 'a t -> bool
    val encoding : 'a Encoding.t -> 'a t Encoding.t
  end
  include (Raw : SIGNATURE)
end

module TaskClockedQueue = struct

  module Raw = struct
    module TMap = XMap.Make(Ptime)

    type 'a t = {
      clock : Ptime.t ;
      queue : 'a list TMap.t ;
    }
    [@@deriving ez]


    let empty clock =
      let queue = TMap.empty in
      make ~clock ~queue
    
    let size x = TMap.fold (fun _ v acc -> List.length v + acc) (queue x) 0

    let flush_until : Ptime.t -> 'a t -> ((Ptime.t * 'a) list * 'a t) = fun ts t ->
      t
      |> set_clock ts
      |> (fun t ->
        let todos =
          t
          |> queue
          |> TMap.filter (fun k _v -> k < ts)
          |> TMap.to_list
          |> List.map (fun (x , lst) -> List.map (fun y -> (x , y)) lst)
          |> List.concat
        in
        let remaining =
          t
          |> queue
          |> TMap.filter (fun k _ -> k >= ts)
        in
        (todos , set_queue remaining t)
      )
    let add_task time task t =
      t |> map_queue (fun tmap ->
        tmap
        |> TMap.update time (fun lst ->
          lst
          |> Option.value ~default:[]
          |> fun lst -> task :: lst (* DFS*)
          |> Option.some
          (* |> (fun lst -> lst @ task) (* BFS*) (* TODO: replace by dequeue*) *)
        )
      )
  end

  module type TYPE = sig
    type 'a t
    val size : 'a t -> int
    val empty : Ptime.t -> 'a t
    val flush_until : Ptime.t -> 'a t -> ((Ptime.t * 'a) list * 'a t)
    val add_task : Ptime.t -> 'a -> 'a t -> 'a t
  end

  include (Raw : TYPE)
end

