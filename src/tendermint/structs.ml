[@@@warning "-23-34"]

open Das_helpers
open Transition

module TCQ = TaskClockedQueue
module Addr = Account.Address
module Sig = Account.Signature

let do_hash : bytes -> bytes = Do_hash.blake2b

module Round = struct
  module Index = Index.Make()
  module Step = struct
    type t =
    | Propose
    | Prevote
    | Precommitment
    [@@deriving ez]
  end
  type t = {
    step : Step.t ;
    duration : Ptime.Span.t ;
  }
  [@@deriving ez]
end

module Commitment = struct
  type unsigned = {
    block_hash : Hash'.t ;
    height : Height.t ;
  }
  [@@deriving ez , ord, show { with_path = false }]
  let unsigned_encoding : unsigned Encoding.t = Encoding.(
    conv (fun (x,y) -> unsigned_make_tpl x y) unsigned_destruct @@
    tuple_2 Hash'.encoding Height.encoding
  )
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding
  let unsigned_to_hash x = x |> unsigned_to_bytes |> do_hash
  let unsigned_pp = pp_unsigned
  type t = {
    unsigned : unsigned ;
    committer_signature : unsigned Sig.t ;
  }
  [@@deriving ez]
  (* TODO *)
  let pp ppf _ = Format.fprintf ppf "commit"
end


module Block = struct
  module Header = struct
    type t = {
      network_name : string ;
      height : Height.t ;
      time : Ptime.t ;
      previous_hash : Hash'.t ;
      (* state_hash : Hash'.t ; *)
    }
    [@@deriving ez]
    (* let encoding = Encoding.(
      conv (fun (x1, x2, x3, x4, x5) -> make_tpl x1 x2 x3 x4 x5) destruct @@
      tuple_5 string Height.encoding XPtime.encoding Hash'.encoding Hash'.encoding
    ) *)
    let encoding = Encoding.(
      conv (fun (x1, x2, x3, x4) -> make_tpl x1 x2 x3 x4) destruct @@
      tuple_4 string Height.encoding XPtime.encoding Hash'.encoding
    )
  end
  
  module Previous_commitments = struct
    (*
      Commitment from previous blocks
    *)
    type t = {
      signatures : Commitment.unsigned Sig.t list ;
    }
    [@@deriving ez]
    let encoding : t Encoding.t = Encoding.(
      conv make_tpl destruct @@ dummy []
    )
  end
  type t = {
    header : Header.t ;
    operations : Bunch.t ;
    previous_commitments : Previous_commitments.t ;
  }
  [@@deriving ez]
  let pp : _ -> t -> unit = fun ppf t ->
    Format.fprintf ppf "Block#%a"
      Height.pp (t |> header |> Header.height)
  let encoding = Encoding.(
    conv make_tpl' destruct @@
    tuple_3 Header.encoding Bunch.encoding Previous_commitments.encoding
  )
  let to_bytes = Encoding.to_bytes encoding
  let hash : t -> bytes = fun t -> Encoding.to_bytes encoding t |> do_hash
  let height : t -> Height.t = fun x -> x |> header |> Header.height
end

module BlockProposal = struct
  module Unsigned = struct
    type t = {
      height : Height.t ;
      round : Round.Index.t ;
      block : Block.t ;
    }
    [@@deriving ez]
    let encoding = Encoding.(
      conv (fun (x , y , z) -> make_tpl x y z) destruct @@
      tuple_3 Height.encoding Round.Index.encoding Block.encoding
    )
    let to_bytes = Encoding.to_bytes encoding
    let to_hash x = x |> to_bytes |> do_hash
  end

  type t = {
    unsigned : Unsigned.t ;
    proposer_signature : Unsigned.t Sig.t ;
  }
  [@@deriving ez]

  let height t = t |> unsigned |> Unsigned.height
  let block t = t |> unsigned |> Unsigned.block
  let round t = t |> unsigned |> Unsigned.round
  let to_unsigned_hash t = t |> unsigned |> Unsigned.to_hash

  let pp ppf t =
    Format.fprintf ppf "Block <%a> signed by %a"
      Height.pp (t |> height)
      Addr.pp_public_key (t |> proposer_signature |> Sig.signer)
end

module Blocks = struct

  (* Map per height *)
  module HMap = Height.Map
  (* Map per block hash *)
  module BMap = XMap.Make(Hash')

  module Height_content = struct
    type t = {
      all : Block.t BMap.t ;
      finalized_hash : Hash'.t option ;
    }
    [@@deriving ez]

    let find_opt k t = t |> all |> BMap.find_opt k
    let empty = make_tpl BMap.empty Option.none
    let add k v t = t |> map_all (BMap.add k v)
  end
  type height_content = Height_content.t

  (*
    Double indexed map.
    - At least one block should be added per height (else, memory leaks!)
  *)
  type t = height_content HMap.t

  (* Keep blocks no longer than this duration *)
  let keep_height = 10l

  let empty : t = HMap.empty

  let find_opt : Height.t -> Hash'.t -> t -> Block.t option = fun h bh t ->
    t
    |> HMap.find_opt h
    |> XOption.bind' (Height_content.find_opt bh)
  let find_final : Height.t -> t -> Hash'.t option = fun h t ->
    t
    |> HMap.find_opt h
    |> XOption.bind' Height_content.finalized_hash
    
  let mem h bh t = Option.is_some @@ find_opt h bh t

  (* The block must always exist (for instance, a locked block) *)
  let find : Height.t -> Hash'.t -> t -> Block.t = fun h bh t ->
    t |> find_opt h bh |> Option.get

  let add : Block.t -> t -> t = fun b t ->
    let h = b |> Block.header |> Block.Header.height in
    let bh = Block.hash b in
    t
    |> HMap.remove (Height.map_int32 (fun h -> Int32.sub h keep_height) h)
    |> HMap.update h (fun bmap_opt ->
      let bmap = Option.value ~default:Height_content.empty bmap_opt in
      bmap
      |> Height_content.add bh b
      |> Option.some
    )
end

module Prevote = struct
  type content =
  | Nil of unit
  | Block of Hash'.t
  [@@deriving ez , show { with_path = false } , ord]
  let content_encoding = Encoding.(
    union [
      case get_nil_opt nil unit ;
      case get_block_opt block Hash'.encoding ;
    ]
  )
  let nil = nil ()

  type unsigned = {
    content : content ;
    height : Height.t ;
    round : Round.Index.t ;
  }
  [@@deriving ez , show { with_path = false } , ord]
  let unsigned_encoding = Encoding.(
    conv (fun (x,y,z) -> unsigned_make_tpl x y z) unsigned_destruct @@
    tuple_3 content_encoding Height.encoding Round.Index.encoding
  )
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding
  let unsigned_pp = pp_unsigned
  let unsigned_to_hash x = x |> unsigned_to_bytes |> do_hash

  type t = {
    unsigned : unsigned ;
    prevoter_signature : unsigned Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez , show { with_path = false }]
end

module Precommitment = struct
  type unsigned = {
    block_hash : Hash'.t ;
    height : Height.t ;
  }
  [@@deriving ez , show { with_path = false } , ord]
  let unsigned_encoding : unsigned Encoding.t = Encoding.(
    conv (fun (x,y) -> unsigned_make_tpl x y) unsigned_destruct @@
    tuple_2 Hash'.encoding Height.encoding
  )
  let unsigned_to_bytes = Encoding.to_bytes unsigned_encoding
  let unsigned_to_hash x = x |> unsigned_to_bytes |> do_hash
  type t = {
    unsigned : unsigned ;
    precommitter_signature : unsigned Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez]

  (* TODO *)
  let pp ppf _ = Format.fprintf ppf "precommit"
end

module BlockInfo = struct
  type t = {
    block : Block.t ;
    endorser_signature : Block.t Sig.t ; [@printer Sig.pp]
  }
  [@@deriving ez , show { with_path = false }]
  let block_to_bytes = Block.to_bytes
  let block_to_hash x = x |> block_to_bytes |> do_hash
end

module Message = struct
  (* 
    `Block_proposal`, `Prevote` and `Precommitment` are Tendermint messages.
    `Block` is just the block, to populate the db.
  *)
  type t =
  | Block_proposal of BlockProposal.t
  | Prevote of Prevote.t
  | Precommitment of Precommitment.t
  | Commitment of Commitment.t
  | Block of BlockInfo.t
  [@@deriving ez]

  let prefix str f ppf x =
    Format.fprintf ppf "@[%s%a@]" str f x

  let pp : _ -> t -> unit = fun ppf ->
    destruct
    ~block_proposal:(prefix "Block_proposal: " (BlockProposal.pp) ppf)
    ~prevote:(prefix "Prevote: " (Prevote.pp) ppf)
    ~precommitment:(prefix "Precommitment: " (Precommitment.pp) ppf)
    ~commitment:(prefix "Commitment: " (Commitment.pp) ppf)
    ~block:(prefix "Block: " (BlockInfo.pp) ppf)
end