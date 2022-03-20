open Das_helpers
open Oru_virtual

let virtual_batch
: Oru_dummy_raw.Oru_param.Batch.t -> Oru_virtual.Oru_param.Batch.t
= fun x -> Obj.magic x

let raw_batch
: Oru_virtual.Oru_param.Batch.t -> Oru_dummy_raw.Oru_param.Batch.t
= fun x -> Obj.magic x

let virtual_proof
: Oru_dummy_raw.Oru_param.Proof.t -> Oru_virtual.Oru_param.Proof.t
= fun x -> Obj.magic x

let raw_proof
: Oru_virtual.Oru_param.Proof.t -> Oru_dummy_raw.Oru_param.Proof.t
= fun x -> Obj.magic x

let simple_submit = fun () ->
  let s = Oru_virtual.Transition.State.mk_empty () in
  let batch = virtual_batch () in
  let op = Structs.Operation.Submit_batch batch in
  assert (List.length (Transition.State.current_batches_rev s) = 0) ;
  let Transition.{ state = s ; _ } = Transition.do_operation op s in
  assert (List.length (Transition.State.current_batches_rev s) = 1) ;
  ()

let simple_submit_and_flush = fun () ->
  let s = Oru_virtual.Transition.State.mk_empty () in
  let batch = virtual_batch () in
  let op = Structs.Operation.Submit_batch batch in
  let Transition.{ state = s ; _ } = Transition.do_operation op s in
  let s = Transition.flush_block s in
  assert (List.length (Transition.State.current_batches_rev s) = 0) ;
  assert (
    Transition.State.batch_get Height.zero Structs.BatchIndex.zero s = batch
  ) ;
  ()

let () =
  Printexc.record_backtrace true ;
  Test_helpers.run "ORU TX" [
    ("simple.submit" , simple_submit) ;
    ("simple.submit and flush" , simple_submit_and_flush) ;
  ]