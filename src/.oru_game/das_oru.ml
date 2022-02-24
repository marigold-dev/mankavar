open Das_helpers
open Types

(* Branching factor for trace tree exploration *)
let branching = 10
let branchingL = Int64.of_int branching


module Instance_raw(P : PARAMETER) = struct

type proof = P.proof
let proof_check = P.check

module Player = struct
  type t = Rejector | Committer
  [@@deriving ez]
end
type player = Player.t



module State = struct
  (*
    List.length marks = branching + 1
    or range marks = (List.length marks) + 1
    or (on creation) List.length marks = 2
  *)
  type ongoing = {
    next_player : Player.t ;
    marks : mark list ;
  }
  [@@deriving ez]
  let ongoing_make a b = ongoing_make_tpl Player.Rejector [a ; b]
  
  let range (m : marks) =
    Int64.sub
      (m |> XList.last |> Mark.step_nb)
      (m |> List.hd |> Mark.step_nb)

  type t =
  | Pending_rejector_size
  | Ongoing of ongoing
  | Won of Player.t
  [@@deriving ez]
end

module Move = struct
  (*
    List.length dichotomy = branching
    or range marks = (List.length marks) + 1
    First member must agree with matching member in `marks`
    Last member must agree with matching member in `marks`
  *)
  type dichotomy = {
    splice : Mark.t list ;
    final_proof : proof option ;
  }
  [@@deriving ez]

  let dichotomy_range : dichotomy -> int64 = fun d ->
    Int64.sub
      (d |> splice |> XList.last |> Mark.step_nb)
      (d |> splice |> List.hd |> Mark.step_nb)

  type dichotomy_invalid =
  | Unsorted
  | Size_mismatch
  | Range_mismatch
  | Increment_mismatch
  | Final_proof_mismatch
  | Final_range_mismatch

  let dichotomy_finalize
  : dichotomy -> _ = fun d ->
    PseudoEffect.failer @@ fun { fail } ->
    PseudoEffect.returner @@ fun { return } ->
    let marks = d |> splice in
    if List.length marks <> 2 then return false ;
    if dichotomy_range d <> 1L then fail Final_range_mismatch ;
    let first = d |> splice |> List.hd in
    let last = d |> splice |> XList.last in
    let final_proof =
      Option.fold (d |> final_proof)
      ~none:(fail Final_proof_mismatch) ~some:Fun.id 
    in
    if proof_check
      (first |> Mark.state_hash) final_proof
      (last |> Mark.state_hash)
    then true
    else fail Final_proof_mismatch

  let dichotomy_is_valid : State.marks -> dichotomy -> _ = fun s d ->
    PseudoEffect.failer @@ fun { fail } ->
    let marks = d |> splice in

    let step_nb_compare a b = Mark.(Int64.compare (step_nb a) (step_nb b)) in
    if not @@ XList.is_sorted step_nb_compare marks
    then fail Unsorted ;

    if List.length marks > branching
    then fail Size_mismatch ;

    if
      List.length marks < branching &&
      Int64.of_int (List.length marks) <> Int64.succ (dichotomy_range d)
    then fail Size_mismatch ;

    let first = List.hd marks in
    let last = XList.last marks in
    let first_last_eq a b = Mark.equal a first && Mark.equal b last in
    if Option.is_none @@ XList.find_follow first_last_eq s
    then fail Range_mismatch ;

    let increment_check i a b =
      let inc = Int64.sub b a in
      let r = dichotomy_range d in
      let q = Int64.(div r branchingL) in
      let rem = Int64.(rem r branchingL) in
      let expected_inc = if Int64.of_int i < rem then Int64.succ q else q in
      Int64.equal expected_inc inc
    in
    if not @@ XList.foralli_follow increment_check
      (d |> splice |> List.map Mark.step_nb)
    then fail Increment_mismatch ;

    ()

  type rejector =
  | Submit_size of size * dichotomy
  | Dichotomy of dichotomy
  [@@deriving ez]
  type committer = dichotomy
  type t =
  | Rejector of rejector
  | Committer of committer
  [@@deriving ez]

  let committer ms proof_opt = committer @@ dichotomy_make_tpl ms proof_opt
  let rejector_initial size ms proof_opt =
    rejector @@ submit_size size @@
    dichotomy_make_tpl ms proof_opt
  let rejector ms proof_opt =
    rejector @@ dichotomy @@
    dichotomy_make_tpl ms proof_opt
end

type t = {
  commitment : Bytes.t ;
  previous_commitment : Bytes.t ;
  minimal_size : player * size ;
  state : State.t ;
}
[@@deriving ez]
let is_won t = t |> state |> State.is_won
let make ~previous_commitment:c_prev ~commitment:c s =
  make_tpl c c_prev (Player.committer , s) State.pending_rejector_size
  
let do_move : t -> Move.t -> t = fun t m ->
  if is_won t then failwith "can't play a lost game" ;
  match t |> state , m with
  | Won _ , _ -> assert false
  | Pending_rejector_size , Rejector (Submit_size (s , d)) -> (
    let (_ , ms_size) as ms = t |> minimal_size in
    let (_ , ms_size') as ms' = if s < ms_size then (Player.Rejector , s) else ms in
    let t = t |> set_minimal_size ms' in
    let ongoing =
      State.ongoing_make
        (Mark.make_tpl 0L (t |> previous_commitment))
        (Mark.make_tpl ms_size' (t |> commitment))
    in
    let t = t |> set_state (State.ongoing ongoing) in
    if Result.is_ok @@ Move.dichotomy_is_valid (ongoing |> State.marks) d
    then (
      t |> set_state
        (ongoing |> State.set_marks (Move.splice d) |> State.ongoing)
    ) else t
  )
  | Pending_rejector_size , _ -> failwith "expect rejector to submit size"
  | Ongoing ({next_player = Rejector ; _} as ongoing) ,
      Rejector (Dichotomy d) -> (
    if Result.is_ok @@ Move.dichotomy_is_valid (ongoing |> State.marks) d
    then t |> set_state (
      ongoing
      |> State.set_marks (Move.splice d)
      |> State.set_next_player Committer
      |> State.ongoing
    ) else t
  )
  | Ongoing {next_player = Rejector ; _ } , _ ->
    failwith "expected rejector to play a regular move"
  | Ongoing ({next_player = Committer ; _} as ongoing) ,
      Committer d -> (
    if Result.is_ok @@ Move.dichotomy_is_valid (ongoing |> State.marks) d
    then t |> set_state (
      ongoing
      |> State.set_marks (Move.splice d)
      |> State.set_next_player Rejector
      |> State.ongoing
    ) else t
  )
  | Ongoing {next_player = Committer ; _} , _ ->
    failwith "expected committer to play a regular move"

  let do_win : t -> Player.t -> t = fun t p ->
    t |> set_state (State.won p)

  let timeout : t -> t = fun t ->
    match t.state with
    | Pending_rejector_size -> do_win t Player.committer
    | Ongoing {next_player = Committer ; _} -> do_win t Player.rejector
    | Ongoing {next_player = Rejector ; _} -> do_win t Player.committer
    | Won _ -> t
end

module Instance(P : PARAMETER)
: INSTANCE with type proof = P.proof
= Instance_raw(P)