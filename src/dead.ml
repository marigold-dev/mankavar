
(* module RawNode = struct
  module BlockProducerNode : NODE = RawBlockProducerNode
  module EndorserNode : NODE = RawEndorserNode
  type t =
  | Block_producer of BlockProducerNode.t
  | Endorser of EndorserNode.t
  [@@deriving ez]

  let process_message = fun m t ->
    t
    |> destruct
    ~block_producer:(fun x ->
      x
      |> BlockProducerNode.process_message m
      |> fun (x , y) -> x , block_producer y
    )
    ~endorser:(fun x ->
      x
      |> EndorserNode.process_message m
      |> fun (x , y) -> x , endorser y
    )
end

module Node : NODE = RawNode *)

