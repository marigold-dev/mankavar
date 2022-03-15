open Das_helpers
(* open Das_vm *)
open Utils

module Balance = struct
  type t = int64
  let encoding : t Encoding.t = Encoding.int64
  let of_bytes = Encoding.of_bytes encoding
  let to_bytes = Encoding.to_bytes encoding
end

module Patricia = Traced_storage.Sparse.Patricia
module Patricia_consume = Traced_storage.Sparse.Patricia_consume_stream

module type KEY = sig
  type t
  val encoding : t Encoding.t
  val path : Bits.t
end

module type ENCODED = sig
  type t
  val encoding : t Encoding.t
end

module type PATH = sig val path : Bits.t end
let path_of_bits path = (module struct let path = path end : PATH)
module type KVH = sig
  val get : Bits.t -> Bytes.t option
  val set : Bits.t -> Bytes.t -> unit
  val get_hash : unit -> Hash'.t
end

module SConstant(P : PATH)(V : ENCODED)(KVH : KVH) = struct
  open P
  let get () =
    let k = path in
    let bytes_opt = KVH.get k in
    Option.map (Encoding.of_bytes V.encoding) bytes_opt
  let set v =
    let k = path in
    let bytes = Encoding.to_bytes V.encoding v in
    KVH.set k bytes
end

module SMap(P : PATH)(K : ENCODED)(V : ENCODED)(KVH : KVH) = struct
  open P
  let get k =
    let bits = Encoding.to_bits K.encoding k in
    let bytes_opt = KVH.get (path @ bits) in
    Option.map (Encoding.of_bytes V.encoding) bytes_opt
  let set k v =
    let bits = Encoding.to_bits K.encoding k in
    let bytes = Encoding.to_bytes V.encoding v in
    KVH.set (path @ bits) bytes
end

module CI = struct
  type t = Contract_index.t * Int64.t
  let encoding : t Encoding.t =
    let open Encoding in
    tuple_2 Contract_index.encoding XInt64.encoding
end

module STATE_of_KVH(KVH : KVH) : STATE = struct
  let do_hash () = KVH.get_hash ()

  module Ledger = SMap
    (val path_of_bits[false ; false ; false])
    (Account_index)(Balance)(KVH)

  module Contracts = SMap
    (val path_of_bits[false ; false ; true])
    (Contract_index)(Contract)(KVH)

  module Next_account_index = SConstant
    (val path_of_bits[false ; true ; false])
    (Account_index)(KVH)

  module Next_contract_index = struct
    open SConstant
      (val path_of_bits[false ; true ; true])
      (Contract_index)(KVH)
    let get () = get () |> Option.get
    let increment () =
      get () |> Int64.succ |> set
    let get_next () = increment () |> get
  end

  module Memory = SMap
    (val path_of_bits[true ; false ; false])
    (CI)(XInt64)(KVH)

  let get_balance = Ledger.get
  let set_balance = Ledger.set
  let get_contract_exn x = Contracts.get x |> Option.get
  let set_contract_exn k v =
    ignore @@ get_contract_exn k ;
    Contracts.set k v
  let init_contract c =
    let new_index = Next_contract_index.get_next () in
    set_contract_exn new_index c ;
    new_index
  let credit src amount =
    let src_balance = Option.get @@ get_balance src in
    let src_balance' = Int64.add src_balance amount in
    set_balance src src_balance' ;
    ()
  let debit src amount =
    let src_balance = Option.get @@ get_balance src in
    if Int64.compare src_balance amount < 0 then (
      Error ()
    ) else (
      let src_balance' = Int64.sub src_balance amount in
      set_balance src src_balance' ;
      Ok ()
    )
  let read_slow c k = Memory.get (c , k) |> Option.get
  let write_slow c k v = Memory.set (c , k) v
end

module type PARAMETER = sig
  val init : Patricia.t
end

module Make(P : PARAMETER) = struct
  module Raw = struct
    let r = ref P.init
    module KVH = struct
      let get k = Patricia.get_opt !r k |> snd
      let set k v = r := Patricia.set !r k v
      let get_hash () = Patricia.get_hash !r
    end
    include STATE_of_KVH(KVH)
  end
  include (Raw : STATE)
end

module type CONSUME_PARAMETER = sig
  val root : Hash'.t
  val stream : Traced_storage.Stream.Consumer.t
end
module ConsumeMake(P : CONSUME_PARAMETER) = struct
  module Raw = struct
    let stream = ref P.stream
    let tree = ref @@ Patricia_consume.root_hash P.root
    module KVH = struct
      let get k =
        let ((tree' , stream') , bytes) =
          Patricia_consume.get_opt (!tree , !stream) k in
        tree := tree' ;
        stream := stream' ;
        bytes
      let set k v =
        let (tree' , stream') =
          Patricia_consume.set (!tree , !stream) k v in
        tree := tree' ;
        stream := stream' ;
        ()
      let get_hash () = Patricia_consume.get_hash (!tree , !stream)
    end
    include STATE_of_KVH(KVH)
  end
  include (Raw : STATE)
end