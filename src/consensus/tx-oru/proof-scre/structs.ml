open Das_helpers
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


module Contract = struct
  open Das_vm

  type t = {
    program : program ;
    storage : memory ;
  }
  [@@deriving ez , eq]

  let encoding = Encoding.(
    conv make_tpl' destruct @@ tuple_2
      program_encoding memory_encoding
  )
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
  open Das_vm
  
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