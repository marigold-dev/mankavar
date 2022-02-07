open Das_helpers
module TCQ = TaskClockedQueue

(*
  Bad simulated network implementation.
  - Messages take at least `increment` time to be propagated to nodes.
  - All messages are propagated to all nodes
*)

module Ping = struct
  type t =
  | Constant of Ptime.span
  [@@deriving ez]

  let eval : t -> Ptime.span = destruct_tpl
    Fun.id
end

module type PARAMETER = sig
  module Message : sig
    type t
    val pp : Format.formatter -> t -> unit
  end
  val increment : Ptime.span
  val ping : Ping.t
end

module Make(P : PARAMETER) = struct


open P
type message = Message.t

type t = {
  nodes : message Node.Packed.t list ;
  clock : Ptime.t ;
  messages : message TCQ.t ;
}
[@@deriving ez]

let empty clock nodes =
  make ~clock ~nodes ~messages:(TCQ.empty clock)

let run_for : Ptime.span -> t -> t = fun d t ->
  let ns = ref @@ nodes t in
  let cl = ref @@ clock t in
  let msgs = ref @@ messages t in
  let limit = Option.get @@ Ptime.add_span !cl d in
  let inc = increment in
  while Ptime.compare !cl limit <= 0 do
    let todos = ref [] in
    (* First, synchronize. Then, process messages.*)
    (* 1. Synchronize *)
    let ns' = List.map (Node.Packed.map ({ mapper = fun (type a) ((module Node) : (message , a) Node.Packed.node) node ->
      let (todos' , node') = Node.synchronize !cl node in
      List.iter (fun todo -> 
        let ping = Ping.eval ping in
        Format.printf "Ping: %a@;%!" Ptime.Span.pp ping ;
        Format.printf "Inc: %a@;%!" Ptime.Span.pp inc ;
        msgs := TCQ.add_task (Ptime.add_span !cl ping |> Option.get) todo !msgs ;
      ) todos' ;
      node'
    })) !ns in
    ns := ns' ;
    Format.printf "Tasks after sync: %d (%d)@;%!"
      (List.length !todos)
      (TCQ.size !msgs) ;
    List.iter (Format.printf "Will deal with: %a@;" P.Message.pp) !todos ;
    (* 2. Process messages *)
    let (todos' , msgs') = TCQ.flush_until !cl !msgs in
    todos := (todos' |> List.map snd) @ !todos ;
    msgs := msgs' ;
    while !todos <> [] do
      let (next , todos') = List.(hd !todos , tl !todos) in
      Format.printf "Dealing with: %a@;" P.Message.pp next ;
      todos := todos' ;
      let ns' = List.map (Node.Packed.map ({ mapper = fun (type a) (m_node : (message , a) Node.Packed.node) node ->
        let module Node = (val m_node) in
        let (todos' , node') = Node.process_message next node in
        let ping = Ping.eval ping in        
        todos' |> List.iter (fun todo ->
          msgs := TCQ.add_task (Ptime.add_span !cl ping |> Option.get) todo !msgs ;
        ) ;
        node'
      })) !ns in
      ns := ns' ;
    done ;
    cl := Option.get @@ Ptime.add_span !cl inc ;
  done ;
  t
  |> set_nodes !ns
  |> set_clock !cl

end