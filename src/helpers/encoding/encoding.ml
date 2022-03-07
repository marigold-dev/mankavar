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
let case_tpl (forth , back , x) = case forth back x
let union lst = Union lst
let unit = Unit
let int32 = Int32
let int64 = Int64
let string = String
let bytes = Bytes
let conv forth back x = Conv (forth , back , x)
let tuple_2 a b = Tuple_2 (a , b)
let tuple_3 a b c = Tuple_3 (a , b , c)
let tuple_4 a b c d = Tuple_4 (a , b , c , d)
let tuple_5 a b c d e = Tuple_5 (a , b , c , d , e)
let tuple_6 a b c d e f =
  conv (fun ((a , b , c , d , e) , f) -> a , b , c , d , e , f)
  (fun (a , b , c , d , e , f) -> (a , b , c , d , e) , f) @@
  tuple_2 (tuple_5 a b c d e) f
let pair = tuple_2
let int = conv Int64.to_int Int64.of_int int64
let list x = List x
let array a = conv (Array.of_list) (Array.to_list) @@ list a
let option a = union [
  case (fun x -> x) Option.some a ;
  case (function Some _ -> None | None -> Some ()) (fun () -> None) unit ;
]
let dummy default = conv (fun () -> default) (fun _ -> ()) unit

module Size = struct
  let int32 = 4
  let int64 = 8
  let small_int = int32
  let rec main : type a . a t -> a -> int = fun e ->
    match e with
    | Unit -> fun () -> 0
    | Int32 -> fun _ -> int32
    | Int64 -> fun _ -> int64
    | String -> fun s -> small_int + String.length s
    | Bytes -> fun b -> small_int + Bytes.length b
    | Union lst -> fun x -> (
      PseudoEffect.returner @@ fun { return } ->
      List.iter (fun (Case (forth , _back , a)) ->
        match forth x with
        | Some x' -> return @@ small_int + main a x'
        | None -> ()
      ) lst ;
      failwith "no matching case in variant"      
    )
    | List a -> fun lst -> List.fold_left (+) small_int @@ List.map (main a) lst
    | Tuple_2 (a , b) -> fun (x1 , x2) ->
      main a x1 + main b x2
    | Tuple_3 (a , b , c) -> fun (x1 , x2 , x3) ->
      main a x1 + main b x2 + main c x3
    | Tuple_4 (a , b , c , d) -> fun (x1 , x2 , x3 , x4) ->
      main a x1 + main b x2 + main c x3 + main d x4
    | Tuple_5 (a , b , c , d , e) -> fun (x1 , x2 , x3 , x4 , x5) ->
      main a x1 + main b x2 + main c x3 + main d x4 + main e x5
    | Conv (_forth , back , a) -> fun x -> main a @@ back x
end
let size = Size.main

module To_bytes = struct
  let prefix x = Bytes.cat (Bytes.of_string x)

  let main = fun e x ->
    let l = Size.main e x in
    let bytes = Bytes.make l '0' in
    (* Format.printf "size: %d@;%!" l ; *)
    let i = ref 0 in
    let shift n = i := !i + n in
    let of_int32 n =
      shift 4 ;
      Bytes.set_int32_be bytes (!i - 4) n ;
    in
    let of_int64 n =
      shift 8 ;
      Bytes.set_int64_be bytes (!i - 8) n ;
    in
    let of_small_int n = of_int32 @@ Int32.of_int n in
    let of_bytes b =
      let l = Bytes.length b in
      shift l ;
      Bytes.blit b 0 bytes (!i - l) l
    in
    let of_char c =
      shift 1 ;
      Bytes.set bytes (!i - 1) c
    in
    let rec aux : type a . a t -> a -> unit = fun e x ->
      let self = aux in
      match e , x with
      | Unit , () -> ()
      | Int32 , n -> of_int32 n
      | Int64 , n -> of_int64 n
      | String , s ->
        let l = String.length s in
        of_small_int l ;
        s |> String.iter of_char ;
      | Bytes , b ->
        let l = Bytes.length b in
        of_small_int l ;
        of_bytes b ;
      | Union lst , x -> (
        PseudoEffect.returner @@ fun { return } ->
        List.iteri (fun i (Case (forth , _back , a)) ->
          match forth x with
          | Some x' -> return (
            of_small_int i ;
            self a x' ;
          ) 
          | None -> ()
        ) lst ;
        failwith "no matching case in variant"
      )
      | Tuple_2 (a , b) , (x , y) -> (
        self a x ;
        self b y ;
      )
      | Tuple_3 (a , b , c) , (x1 , x2 , x3) -> (
        self a x1 ;
        self b x2 ;
        self c x3 ;
      )
      | Tuple_4 (a , b , c , d) , (x1 , x2 , x3 , x4) -> (
        self a x1 ;
        self b x2 ;
        self c x3 ;
        self d x4 ;
      )
      | Tuple_5 (a , b , c , d , e) , (x1 , x2 , x3 , x4 , x5) -> (
        self a x1 ;
        self b x2 ;
        self c x3 ;
        self d x4 ;
        self e x5 ;
      )
      | List a , lst -> (
        of_small_int @@ List.length lst ;
        lst |> List.iter (self a)
      )
      | Conv (_forth , back , a) , x -> self a @@ back x
    in
    aux e x ;
    bytes

  (* let rec main : type a . a t -> a -> bytes = fun e ->
    let self = main in
    match e with
    | Unit -> fun () -> Bytes.empty
    | Int32 -> fun i -> (
      of_int32 i
    )
    | Int64 -> fun i -> (
      of_int64 i
    )
    | String -> fun s -> (
      Bytes.cat
        (of_int @@ String.length s)
        (Bytes.of_string s)
    )
    | Bytes -> fun b ->
      Bytes.cat (of_int @@ Bytes.length b) b
    | Union lst -> fun x -> (
      PseudoEffect.returner @@ fun { return } ->
      List.iteri (fun i (Case (forth , _back , a)) ->
        match forth x with
        | Some x' -> return (
          Bytes.cat (of_int i) (self a x')
        ) 
        | None -> ()
      ) lst ;
      failwith "no matching case in variant"
    )
    | Tuple_2 (a , b) -> fun (x , y) -> (
      Bytes.cat (self a x) (self b y)
    )
    | Tuple_3 (a , b , c) -> fun (x , y , z) -> (
      Bytes.cat (self a x) @@ Bytes.cat (self b y) (self c z)
    )
    | Tuple_4 (a , b , c , d) -> fun (x , y , z , w) -> (
      Bytes.cat (self a x) @@ Bytes.cat (self b y)
        @@ Bytes.cat (self c z) (self d w)
    )
    | Tuple_5 (a , b , c , d , e) -> fun (x1 , x2 , x3 , x4 , x5) -> (
      Bytes.cat (self a x1) @@ Bytes.cat (self b x2)
        @@ Bytes.cat (self c x3) @@ Bytes.cat (self d x4) (self e x5)
    )
    | List a -> fun lst -> (
      Bytes.cat (of_int @@ List.length lst) @@
      Bytes.concat Bytes.empty @@ List.map (self a) lst
    )
    | Conv (_forth , back , a) -> fun x -> self a @@ back x *)
end

module Of_bytes = struct

  let main = fun a b ->
    let i = ref 0 in
    let shift n = i := !i + n in
    let get_int32 () =
      shift 4 ;
      Bytes.get_int32_be b (!i - 4)
    in
    let get_int64 () =
      shift 8 ;
      Bytes.get_int64_be b (!i - 8)
    in
    let get_small_int () = get_int32 () |> Int32.to_int in
    let rec aux : type a . a t -> a = fun a ->
    match a with
    | Unit -> ()
    | Int32 -> get_int32 ()
    | Int64 -> get_int64 ()
    | String -> (
      let l = get_small_int () in
      shift l ;
      Bytes.to_string @@ Bytes.sub b (!i - l) l
    )
    | Bytes -> (
      let l = get_small_int () in
      shift l ;
      Bytes.sub b (!i - l) l
    )
    | Union lst -> (
      let case_i = get_small_int () in
      let (Case (_forth , back , a')) = List.nth lst case_i in
      back @@ aux a'
    )
    | Tuple_2 (a , b) -> (
      let a' = aux a in
      let b' = aux b in
      a' , b'
    )
    | Tuple_3 (a , b , c) -> (
      let a' = aux a in
      let b' = aux b in
      let c' = aux c in
      a' , b' , c'
    )
    | Tuple_4 (a , b , c , d) -> (
      let a' = aux a in
      let b' = aux b in
      let c' = aux c in
      let d' = aux d in
      a' , b' , c' , d'
    )
    | Tuple_5 (a , b , c , d , e) -> (
      let a' = aux a in
      let b' = aux b in
      let c' = aux c in
      let d' = aux d in
      let e' = aux e in
      a' , b' , c' , d' , e'
    )
    | List a -> (
      let l = get_small_int () in
      List.init l (fun _ -> aux a)
    )
    | Conv (forth , _back , a) -> forth @@ aux a
    in
    aux a

end

let to_bytes = To_bytes.main
let of_bytes = Of_bytes.main