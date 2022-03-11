open Das_vm
open Eval

type memory = Value.t VMap.t
let memory_encoding = Das_helpers.Encoding.(
  conv VMap.of_list VMap.to_list @@ list @@ tuple_2
    int64 int64
)

module type RW_SLOW = sig
  val read_slow : int64 -> int64
  val write_slow : int64 -> int64 -> unit
end
module type R_SLOW = sig
  val read_slow : int64 -> int64
end

module Cache(P : R_SLOW)() = struct
  module Raw = struct
    type cache = (Value.t * bool) VMap.t (* value * to_write *)
    let cache : cache ref = ref VMap.empty
    let read_slow k =
      match VMap.find_opt k !cache with
      | Some v -> fst v
      | None -> (
        let v = P.read_slow k in
        cache := VMap.add k (v , false) !cache ;
        v
      )
    let write_slow k v =
      cache := VMap.add k (v , true) !cache
  end
  module Rw_slow : RW_SLOW = Raw
  let content = Raw.cache  
end

module Make(P : RW_SLOW) = struct
module Raw = struct
  let pp ppf m =
    let print x = Format.fprintf ppf x in
    let lst = m |> VMap.to_seq |> List.of_seq in
    print "@[" ;
    lst |> List.iter (fun (k , v) -> (
      print "%a -> %a@;" Value.pp k Value.pp v
    )) ;
    print "@]" ;
    ()

  let read = 0
  let write = 1
  let read_slow = 2
  let write_slow = 3
  let read_data = 4

  let custom ~store ~input (state : unit state) i = match i with
  | 0 ->
    set_register state B @@
    VMap.find
      (get_register state A)
      !store
  | 1 ->
    store :=
    VMap.add
      (get_register state A)
      (get_register state B)
      !store
  | 2 ->
    set_register state B @@ P.read_slow @@ get_register state A
  | 3 ->
    P.write_slow (get_register state A) (get_register state B)
  | 4 -> 
    let index = Int64.to_int (get_register state A) in
    set_register state B input.(index)
  | _ -> assert false
  [@@inline]
  let eval ~storage ~input program =
    let store : memory ref = ref storage in
    let _state = eval (custom ~store ~input) program () in
    !store
end

include Raw
end