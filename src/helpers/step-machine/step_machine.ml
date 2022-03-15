module Step = struct
  type ('s , 'o) t =
  | Continue of 's
  | Finished of 'o

end
module Step_n = struct
  type ('s , 'o) t =
  | Continue of 's
  | Finished of int * 'o
end

type ('s , 'o) stepper = ('s , 'o) Step.t -> ('s , 'o) Step.t
type ('s , 'o) stepper_n = n:int -> ('s , 'o) Step.t -> ('s , 'o) Step_n.t

let mk_stepper_n (step : ('s , 'o) stepper) : ('s , 'o) stepper_n = fun ~n s ->
  PseudoEffect.returner @@ fun { return } ->
  let sr = ref s in
  for i = 1 to n do
    sr := step !sr ;
    match !sr with
    | Continue _ -> ()
    | Finished o -> return @@ Step_n.Finished (i , o)
  done ;
  match !sr with
  | Step.Continue x -> Step_n.Continue x
  | Finished _ -> assert false


module type STEPPER = sig
  type input
  type state
  type output
  val mk_state : input -> state
  val step : (state , output) stepper
  val step_n : (state , output) stepper_n
  val eval : input -> output
end