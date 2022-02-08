let tuple2 x y = (x , y)
let tuple3 x y z = (x , y, z)

module PseudoEffect = struct

  type 'a return = {
    return : 'b . 'a -> 'b ;
  }

  let returner (type r) f =
    let exception Return of r in
    let p = {
      return = fun x -> raise (Return x) ;
    } in
    try f p
    with Return r -> r

  type 'a fail = {
    fail : 'b . 'a -> 'b ;
  }

  let fail_opt f =
    returner @@ fun { return } ->
    let fail () = return None in
    Option.some @@ f { fail }

end

module Encoding = struct
  (* Not serialiable!! *)
  type 'a t =
  | Unit : unit t
  | Int32 : int32 t
  | Int64 : int64 t
  | String : string t
  | Bytes : bytes t
  | Union : 'a case list -> 'a t
  | List : 'a t -> 'a list t
  | Tuple_2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple_3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple_4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Tuple_5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
  | Conv : ('a -> 'b) * ('b -> 'a) * 'a t -> 'b t
  and 'a case = Case : ('a -> 'b option) * ('b -> 'a) * 'b t -> 'a case

  let case forth back x = Case (forth , back , x)
  let union lst = Union lst
  let unit = Unit
  let int32 = Int32
  let int64 = Int64
  let string = String
  let bytes = Bytes
  let tuple_2 a b = Tuple_2 (a , b)
  let tuple_3 a b c = Tuple_3 (a , b , c)
  let tuple_4 a b c d = Tuple_4 (a , b , c , d)
  let tuple_5 a b c d e = Tuple_5 (a , b , c , d , e)
  let pair = tuple_2
  let conv forth back x = Conv (forth , back , x)
  let int = conv Int64.to_int Int64.of_int int64
  let list x = List x

  module To_bytes = struct
    (* 
      TODO:
      - Add size prefix for unbounded constructors (string, bytes, list)
      - Add tag for unions
    *)
    (*
      TODO:
      - Precompute expected size of buffer
      - Use buffer
    *)
    let prefix x = Bytes.cat (Bytes.of_string x)
    let of_int32 i =
      let b = Bytes.make 4 '0' in
      Bytes.set_int32_be b 0 i ;
      b
    let of_int64 i =
      let b = Bytes.make 8 '0' in
      Bytes.set_int64_be b 0 i ;
      b
    let rec main : type a . a t -> a -> bytes = fun e ->
      let self = main in
      match e with
      | Unit -> fun () -> prefix "u" Bytes.empty
      | Int32 -> fun i -> (
        prefix "i" @@ of_int32 i
      )
      | Int64 -> fun i -> (
        prefix "I" @@ of_int64 i
      )
      | String -> fun s -> (
        prefix "s" @@ Bytes.of_string s
      )
      | Bytes -> fun b -> prefix "b" b
      | Union lst -> fun x -> (
        PseudoEffect.returner @@ fun { return } ->
        List.iter (fun (Case (forth , _back , a)) ->
          match forth x with
          | Some x' -> return @@ self a x'
          | None -> ()
        ) lst ;
        failwith "no matching case in variant"
      )
      | Tuple_2 (a , b) -> fun (x , y) -> (
        prefix "2" @@ Bytes.cat (self a x) (self b y)
      )
      | Tuple_3 (a , b , c) -> fun (x , y , z) -> (
        prefix "3" @@ Bytes.cat (self a x) @@ Bytes.cat (self b y) (self c z)
      )
      | Tuple_4 (a , b , c , d) -> fun (x , y , z , w) -> (
        prefix "4" @@ Bytes.cat (self a x) @@ Bytes.cat (self b y)
          @@ Bytes.cat (self c z) (self d w)
      )
      | Tuple_5 (a , b , c , d , e) -> fun (x1 , x2 , x3 , x4 , x5) -> (
        prefix "5" @@ Bytes.cat (self a x1) @@ Bytes.cat (self b x2)
          @@ Bytes.cat (self c x3) @@ Bytes.cat (self d x4) (self e x5)
      )
      | List a -> fun lst -> (
        prefix "l" @@ Bytes.concat Bytes.empty @@ List.map (self a) lst
      )
      | Conv (_forth , back , a) -> fun x -> self a @@ back x
  end
  let to_bytes = To_bytes.main
end

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
end

module XMap = struct
  module type S = sig
    include Map.S
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
  end
  module Make(P : Map.OrderedType) : S with type key = P.t = struct
    include Map.Make(P)
    let to_list t =
      t
      |> to_seq
      |> List.of_seq
    
    let of_list lst =
      lst
      |> List.to_seq
      |> of_seq
  end
end

module XOption = struct
  let value' : (unit -> 'a) -> 'a option -> 'a = fun f opt ->
    match opt with
    | None -> f ()
    | Some x -> x
end

module XBytes = struct
  let pp_char ppf c = Format.fprintf ppf "%x" @@ Char.code c
  let pp ppf b =
    Bytes.iter (pp_char ppf) b
end

module Hash = struct
  type t = Bytes.t
  let compare : t -> t -> int = Bytes.compare
  let pp = XBytes.pp
  let encoding : t Encoding.t = Encoding.bytes
end

module Index = struct
  module Raw = struct
    type t =
    | Height of int32
    [@@deriving ez]

    let of_int32 = height
    let to_int32 = get_height_exn
    let map_int32 f x = of_int32 @@ f @@ to_int32 x
    let compare : t -> t -> int = fun x y ->
      Int32.compare (to_int32 x) (to_int32 y)
    let equal a b = compare a b = 0
    let zero = height Int32.zero
    let increment t =
      t
      |> map_height Int32.succ

    module Map = XMap.Make(struct type nonrec t = t let compare = compare end)
    let pp ppf x = Format.fprintf ppf "%ld" (x |> get_height_exn)
    let encoding = Encoding.(conv of_int32 to_int32 int32)
  end

  module Make() : sig
    type t
    val of_int32 : int32 -> t
    val to_int32 : t -> int32
    val map_int32 : (int32 -> int32) -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val zero : t
    val increment : t -> t
    val pp : Format.formatter -> t -> unit
    val encoding : t Encoding.t
    module Map : module type of XMap.Make(struct type nonrec t = t let compare = compare end)
  end = struct include Raw end
end

module Height = Index.Make()

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

