open Das_helpers
open Das_vm
open Eval


module Contract_index = XInt64
module Key_index = XInt64
module Account_index = struct
  type t =
  | Key_index of Key_index.t
  | Contract_index of Contract_index.t
  [@@deriving ez , ord]

  let key_encoding = Encoding.int64
  let encoding = Encoding.(
    union [
      case get_key_index_opt key_index key_encoding ;
      case get_contract_index_opt contract_index Contract_index.encoding ;
    ]
  )
  let to_bits = Encoding.to_bits encoding
  let pp = Fun.flip @@ destruct_tpl XFormat.int64' Contract_index.pp'
end

let account_key x = Account_index.Key_index x


type memory = Value.t VMap.t
let equal_memory = VMap.equal Value.eq
let memory_encoding = Das_helpers.Encoding.(
  conv VMap.of_list VMap.to_list @@ list @@ tuple_2
    int64 int64
)
let memory_pp = VMap.pp Value.pp Value.pp

module Contract = struct
  type t = {
    program : program ;
    storage : memory ;
  }
  [@@deriving ez , eq]

  let program_encoding = Encoding.(
    conv program_make_tpl' program_destruct @@ tuple_2
      (list rt_instruction_encoding)
      int
  )

  let encoding = Encoding.(
    conv make_tpl' destruct @@ tuple_2
      program_encoding memory_encoding
  )
end

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

module Eval = struct
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
end

module type STATE = sig
  val do_hash : unit -> Hash'.t

  val get_balance : Account_index.t -> int64 option
  val credit : Account_index.t -> int64 -> unit
  val debit : Account_index.t -> int64 -> (unit , unit) result
  val init_key : int64 -> Key_index.t
  val get_contract_exn : Contract_index.t -> Contract.t
  val set_contract_exn : Contract_index.t -> Contract.t -> unit
  val init_contract : Contract.t -> int64 -> Contract_index.t
  val read_slow : Contract_index.t -> int64 -> int64
  val write_slow : Contract_index.t -> int64 -> int64 -> unit
  val init : unit -> unit
end

module Destination = struct
  type t =
  | Account of Account_index.t
  | Public_key of Crypto.Address.t
end

module Transfer = struct
  type t = {
    source : Account_index.t ;
    destination : Account_index.t ;
    amount : int64 ;
    payload : int64 array ;
    max_gas : int64 ;
  }
  [@@deriving ez]

  let get_max_gas = max_gas
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_5
      Account_index.encoding Account_index.encoding
      int64 (array int64) int64 
  )
  let payload_pp = XFormat.array XFormat.int64
  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Transfer:@;%a -> %a@;"
      Account_index.pp (source t) Account_index.pp (destination t) ;
    Format.fprintf ppf "%Ld tokens with payload %a and max gas %Ld@;@]"
      (amount t) payload_pp (payload t) (max_gas t)
  let pp' = Fun.flip pp
end

module Origination = struct
  type t = {
    source : Account_index.t ;
    amount : int64 ;
    contract : Contract.t ;
    slow_memory : Value.t VMap.t ;
    max_gas : int64 ;
  }
  [@@deriving ez]
  let get_max_gas = max_gas
  let slow_memory_pp = XFormat.(
    conv VMap.to_list @@ list @@ tuple_2 int64 int64
  )
  let slow_memory_encoding = Encoding.(
    conv VMap.of_list VMap.to_list @@ list @@ tuple_2 int64 int64
  )
  let storage_encoding = Encoding.(
    conv VMap.of_list VMap.to_list @@ list @@ tuple_2 int64 int64
  )
  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Origination:@;%Ld@;"
      (amount t) ;
    Format.fprintf ppf "Memory %a and max gas %Ld@;@]"
      slow_memory_pp (slow_memory t) (max_gas t)
  let pp' = Fun.flip pp
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_5
      Account_index.encoding int64 Contract.encoding
      slow_memory_encoding int64 
  )
  let size = Encoding.size encoding
end

module Operation = struct
  type t =
  | Transfer of Transfer.t
  | Origination of Origination.t
  [@@deriving ez]

  let get_max_gas = destruct_tpl
    Transfer.get_max_gas Origination.get_max_gas
  
  let pp = Fun.flip @@ destruct_tpl
    Transfer.pp' Origination.pp'
  let encoding = Encoding.(
    union [
      case get_transfer_opt transfer Transfer.encoding ;
      case get_origination_opt origination Origination.encoding ;
    ]
  )
  let do_hash' t = Crypto.blake2b @@ Encoding.to_bytes encoding t 
  let do_hash = Hash.make do_hash'
  let size = Encoding.size encoding
end