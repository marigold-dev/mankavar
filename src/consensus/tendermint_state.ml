open Das_helpers
open Das_network

module TCQ = TaskClockedQueue
module Addr = Crypto.Address
module Sig = Crypto.Signature
module Send = Node.Send

open Structs

let signature_prepend sign lst =
  if List.exists (Sig.equal sign) lst
  then lst
  else sign :: lst

module BlockchainState = struct
  module Hash = struct
    type t = unit
  end
end

module Lock = struct
  (* All of the `prevote_signatures` should be for `content` *)
  (* The list `prevote_signatures` must have size > 2/3 of validators *)
  type t = {
    content : Prevote.unsigned ;
    prevote_signatures : Prevote.prevoter_signature list ;
  }
  [@@deriving ez]

  let pp ppf x =
    let (c , sigs) = destruct x in
    Format.fprintf ppf "(%d sigs)%a"
      (List.length sigs)
      Prevote.unsigned_pp c

  let height t = t |> content |> Prevote.height
end

module ProposalState = struct
  (* This only matters when the node has no lock *)
  type t =
  | Proposed of BlockProposal.t
  | No_proposal
  [@@deriving ez]
end

module PrevoteState = struct
  module CMap = XMap.Make(struct
    type t = Prevote.content
    let compare = Prevote.compare_content
  end)

  (* All signatures for a given Prevote.content should be different *)
  type t = {
    prevotes : Prevote.prevoter_signature list CMap.t ;
    threshold : Lock.t Option.t ;
  }
  [@@deriving ez]

  let empty threshold = make ~prevotes:CMap.empty ~threshold

  let add ~threshold : Prevote.t -> t -> t = fun pv t ->
    let sign = pv |> Prevote.prevoter_signature in
    let key = pv |> Prevote.unsigned |> Prevote.content in
    (* Format.printf "add prevote@;%!" ; *)
    t
    |> map_prevotes (CMap.update key (fun lst_opt ->
      let lst = Option.value lst_opt ~default:[] in
      Option.some @@
      if List.exists (Sig.equal sign) lst
      then lst
      else sign :: lst
    ))
    |> fun t ->
    let signs = CMap.find key (t |> prevotes) in
    (* Format.printf "Prevote Signatures (%d) vs Threshold (%d)@;%!" (List.length signs) threshold ; *)
    if List.length signs >= threshold
    then t |> set_threshold (
      signs
      |> Lock.make_tpl (pv |> Prevote.unsigned)
      |> Option.some
    ) else t

end

module PrecommitmentState = struct
  module CMap = XMap.Make(struct
    type t = Precommitment.unsigned
    let compare = Precommitment.compare_unsigned
  end)

  type t = {
    precommits : Precommitment.precommitter_signature list CMap.t ;
    precommitted_block : Precommitment.unsigned Option.t ;
  }
  [@@deriving ez]

  let empty = make_tpl CMap.empty Option.none

  let add ~threshold : Precommitment.t -> t -> t = fun pv t ->
    let sign = pv |> Precommitment.precommitter_signature in
    let key = pv |> Precommitment.unsigned in
    t
    |> map_precommits (CMap.update key (fun lst_opt ->
      let lst = Option.value lst_opt ~default:[] in
      Option.some @@
      if List.exists (Sig.equal sign) lst
      then lst
      else sign :: lst
    ))
    |> fun t ->
    let signs = CMap.find key (t |> precommits) in
    if List.length signs >= threshold
    then t |> set_precommitted_block (
      pv |> Precommitment.unsigned
      |> Option.some
    ) else t
end

module Commitments = struct
  (* Map per commitment *)
  module CMap = XMap.Make(struct
    type t = Commitment.unsigned
    let compare = Commitment.compare_unsigned
  end)
  type t = Commitment.unsigned Sig.t list CMap.t
  (* Keep commitments no longer than this duration *)
  let keep_height = 10l
  let empty : t = CMap.empty
  let add : Commitment.t -> t -> t = fun c t ->
    (* Format.printf "Add commitment:%a@;%!" Commitment.unsigned_pp (c |> Commitment.unsigned) ; *)
    (* Format.printf "Number of commitments:%d@;%!" @@ CMap.cardinal t ; *)
    let unsigned = c |> Commitment.unsigned in
    let sigs =
      t |> CMap.find_opt unsigned
      |> Option.value ~default:[]
    in
    let sig_ = c |> Commitment.committer_signature in
    let sigs' = signature_prepend sig_ sigs in
    (* Format.printf "%d vs %d@;%!" (List.length sigs) (List.length sigs') ; *)
    t |> CMap.add unsigned sigs'
  let find_opt : Commitment.unsigned -> t -> Commitment.unsigned Sig.t list option
  = fun h t ->
    CMap.find_opt h t
end

module CommitmentState = struct


  (* The committed block is always at the current height *)
  type t = {
    commits : Commitments.t ;
    committed_block : Hash'.t option ;
  }
  [@@deriving ez]
  let empty = make_tpl Commitments.empty Option.none
  let all_commitments_nb t = Commitments.CMap.cardinal (t |> commits)
  let add ~threshold : Commitment.t -> t -> t = fun c t ->
    t
    |> map_commits (Commitments.add c)
    |> fun t ->
    let sigs = t |> commits |> Commitments.CMap.find (c |> Commitment.unsigned) in
    let bh = c |> Commitment.unsigned |> Commitment.block_hash in
    (* Format.printf "Commitment Signatures (%d) vs Threshold (%d)@;%!" (List.length sigs) threshold ; *)
    (* Format.printf "Number of commitments:%d@;%!" @@ all_commitments_nb t ; *)
    if List.length sigs >= threshold
    then t |> set_committed_block @@ Option.some bh
    else t

  let find_opt u t = t |> commits |> Commitments.find_opt u

end

module PostcommitmentState = struct
  type t = Hash'.t
end

module StepState = struct
  type t =
  | Proposal of ProposalState.t
  | Prevote of PrevoteState.t
  | Precommitment of PrecommitmentState.t
  | Commitment of unit
  | Postcommitment of PostcommitmentState.t
  [@@deriving ez]

  let kind = destruct_tpl
    (fun _ -> 0)
    (fun _ -> 1)
    (fun _ -> 2)
    (fun _ -> 3)
    (fun _ -> 4)
  let compare_kind : t -> t -> int = fun a b -> compare (kind a) (kind b)
  let pp ppf = destruct_tpl
    (fun _ -> Format.fprintf ppf "proposal")
    (fun _ -> Format.fprintf ppf "prevote")
    (fun _ -> Format.fprintf ppf "precommitment")
    (fun _ -> Format.fprintf ppf "commitment")
    (fun _ -> Format.fprintf ppf "postcommitment")

  let prevote_empty = prevote @@ PrevoteState.empty Option.none
end