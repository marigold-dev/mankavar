open Das_helpers

type mark = Mark.t
type size = int64

module type PARAMETER = sig
  type proof
  val check : bytes -> proof -> bytes -> bool
end

module type INSTANCE = sig
  type t
  type proof
  module Move : sig
    type t
    val committer : Mark.t list -> proof option -> t
    val rejector : Mark.t list -> proof option -> t
    val rejector_initial : size -> Mark.t list -> proof option -> t
  end
  val make : previous_commitment:Bytes.t -> commitment:Bytes.t -> size -> t
  val do_move : t -> Move.t -> t
  val timeout : t -> t
end

(*
  Blockchain Proof VM
  - Blockchain: operation+interstate
  - Proof: proof
*)
module type BPVM = sig
  type state
  type proof
  type interstate
  type operation

  val interstate_make : state -> operation -> interstate
  val interstate_hash : interstate -> interstate hash
  type step_result =
  | Next of interstate
  | Halt of state
  val step : interstate -> step_result
  type step_proof_result =
  | PNext of interstate hash
  | PHalt of state hash
  | Proof_mismatch
  val step_proof : bytes -> proof -> step_proof_result
end

