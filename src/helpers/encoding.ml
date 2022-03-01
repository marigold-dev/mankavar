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
let tuple_2 a b = Tuple_2 (a , b)
let tuple_3 a b c = Tuple_3 (a , b , c)
let tuple_4 a b c d = Tuple_4 (a , b , c , d)
let tuple_5 a b c d e = Tuple_5 (a , b , c , d , e)
let pair = tuple_2
let conv forth back x = Conv (forth , back , x)
let int = conv Int64.to_int Int64.of_int int64
let list x = List x
let array a = conv (Array.of_list) (Array.to_list) @@ list a
let dummy default = conv (fun () -> default) (fun _ -> ()) unit

module Size = struct
  let rec main : type a . a t -> a -> int = fun e ->
    match e with
    | Unit -> fun () -> 1
    | Int32 -> fun _ -> 4
    | Int64 -> fun _ -> 8
    | String -> fun s -> String.length s
    | Bytes -> fun b -> Bytes.length b
    | Union lst -> fun x -> (
      PseudoEffect.returner @@ fun { return } ->
      List.iter (fun (Case (forth , _back , a)) ->
        match forth x with
        | Some x' -> return @@ main a x'
        | None -> ()
      ) lst ;
      failwith "no matching case in variant"      
    )
    | List a -> fun lst -> List.fold_left (+) 0 @@ List.map (main a) lst
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