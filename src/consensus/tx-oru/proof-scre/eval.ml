(* open Das_helpers *)
open Das_vm

type memory = Value.t VMap.t
let equal_memory = VMap.equal Value.eq
let memory_encoding = Das_helpers.Encoding.(
  conv VMap.of_list VMap.to_list @@ list @@ tuple_2
    int64 int64
)
let memory_pp = VMap.pp Value.pp Value.pp

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
module type CACHE = module type of (Cache(val (Obj.magic ()) : R_SLOW))()

let read = 0
let write = 1
let read_slow = 2
let write_slow = 3
let read_data = 4

module C_like = struct
  open Das_vm.C_like
  let read = call_custom1 read
  let write = call_custom2 write
  let read_slow = call_custom1 read_slow
  let write_slow = call_custom2 write_slow
  let read_data = call_custom1 read_data
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

    open Eval

    let custom ~store ~input (state : unit state) i = match i with
    | 0 ->
      set_register state A @@
      VMap.find
        (get_register state A)
        !store
    | 1 ->
      Format.printf "writing (%Ld , %Ld)!!@;%!"
        (get_register state A) (get_register state B)
      ;
      store :=
      VMap.add
        (get_register state A)
        (get_register state B)
        !store
    | 2 ->
      set_register state A @@ P.read_slow @@ get_register state A
    | 3 ->
      P.write_slow (get_register state A) (get_register state B)
    | 4 -> 
      let index = Int64.to_int (get_register state A) in
      Format.printf "reading data!!@;%!" ;
      set_register state A input.(index)
    | _ -> assert false
    [@@inline]

    module Step_n = struct
      type step_n =
      | Finished of memory * int
      | Pending of memory * unit state
      let main ~n ~storage ~input s =
        let store : memory ref = ref storage in
        match step_n (custom ~store ~input) ~n s with
        | Finished n' -> Finished (!store , n')
        | Pending s -> Pending (!store , s)
    end
    let empty_state p = empty_state p ()
    let step_n = Step_n.main
    let step_until_stop ~storage ~input s =
      let store : memory ref = ref storage in
      ignore @@ step_until_stop (custom ~store ~input) s ;
      !store
  end

  include Raw
end

