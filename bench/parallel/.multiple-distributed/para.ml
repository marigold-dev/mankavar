module H = Das_helpers

let bytes_pp_hex ppf b =
  b |> Bytes.iter @@ fun c ->
    let i = Char.code c in
    Format.fprintf ppf "%x" i ;
    ()

let public_key_pp ppf x =
  Format.fprintf ppf "%a" bytes_pp_hex @@
  Sodium.Sign.Bytes.of_public_key x

module Signature_request = struct
  type t = {
    pk : Sodium.Sign.public_key ;
    signature : bytes ;
    content : bytes ;
  }
  [@@deriving ez]
  
  let pp ppf t =
    let pk , _sign , bytes = destruct t in
    Format.printf "Signature Request <%a , %a>"
      public_key_pp pk bytes_pp_hex bytes
  
  let process t =
    let pk , sign , bytes = destruct t in
    let bytes' = Sodium.Sign.Bytes.sign_open pk sign in
    bytes = bytes'
end

let random_ok_sig () =
  let sk , pk = Sodium.Sign.random_keypair () in
  let bytes = Sodium.Random.Bytes.generate 10 in
  let signature = Sodium.Sign.Bytes.sign sk bytes in
  (pk , signature , bytes)

let random_bad_sig () =
  let sk , pk = Sodium.Sign.random_keypair () in
  let bytes = Sodium.Random.Bytes.generate 10 in
  let signature = Sodium.Sign.Bytes.sign sk bytes in
  Bytes.set signature 0 @@ H.XChar.map_int Int.succ (Bytes.get signature 0) ;
  (pk , signature , bytes)

let data = Array.init 10 (fun _ -> random_ok_sig ())

module ISet = Set.Make(Int)
module M = struct

  type check_failed = int list

  type t =
  | Signature_requests of (int * Signature_request.t list)
  | Signature_responses of (int * check_failed)
  | Stop_slave_after of int list
  [@@deriving ez]

  let pp ppf t = t |> destruct_tpl
    H.XFormat.(pair int (list Signature_request.pp) ppf)
    H.XFormat.(pair int (list int) ppf)
    H.XFormat.(prefix "Stop Slave" (list int) ppf)

  let string_of_message = Format.asprintf "%a" pp
end
module L = struct
  let msg x = Logs_lwt.msg ?src:None x
end

module Make() = struct
module D = Distributed_lwt.Make(M)(L)
module XD = struct
  open D
  let map_lst f lst =
    let rec aux acc lst = match lst with
    | [] -> return @@ List.rev acc
    | hd :: tl -> f hd >>= fun hd' -> aux (hd' :: acc) tl
    in
    aux [] lst
  let iteri_lst f lst =
    let rec aux i lst = match lst with
    | [] -> return ()
    | hd :: tl -> f i hd >>= fun () -> aux (i + 1) tl
    in
    aux 0 lst
  let iter_lst f lst =
    let rec aux lst = match lst with
    | [] -> return ()
    | hd :: tl -> f hd >>= fun () -> aux tl
    in
    aux lst
end

exception Unexpected_msg

module Slave = struct
  let config _ =
    D.Local {
      node_name = "slave" ; 
    }  

  let proc master_pid () =
    let open D in
    Format.printf "Slave node started@;%!" ;
    let todo = ref ISet.empty in
    let done_ = ref ISet.empty in
    receive_loop @@ case @@ function 
    | M.Signature_requests (id , reqs) -> Option.some @@ fun () ->
      let failed = ref [] in
      reqs |> List.iteri (fun i req ->
        if not @@ Signature_request.process req then (
          failed := i :: !failed
        )
      ) ;
      send master_pid @@ M.signature_responses (id , !failed) >>= fun () ->
      if ISet.is_empty !todo then (
        done_ := ISet.add id !done_ ;
        return true
      ) else (
        todo := ISet.remove id !todo ;
        return @@ not @@ ISet.is_empty !todo
      )
    | M.Signature_responses _ -> Option.some @@ fun () ->
      fail Unexpected_msg
    | M.Stop_slave_after ids -> (
      Option.some @@ fun () ->
      todo := ISet.of_list ids ;
      todo := ISet.diff !todo !done_ ;
      done_ := ISet.empty ;
      return @@ not @@ ISet.is_empty !todo
    )
end

module Master = struct
  let nb_slaves = 4
  let batch_size = 100
  let config =
    D.Local {
      node_name = "master" ; 
    }

  let parallel_check requests =
    let open D in

    Format.printf "Line %s@;%!" __LOC__ ;

    get_remote_nodes >>= fun nodes ->
    get_self_pid >>= fun pid_to_send_to ->
    get_self_node >>= fun my_node ->

    Format.printf "Line %s@;%!" __LOC__ ;

    (* spawn and monitor a process on the remote node separately *)

    nodes |> XD.map_lst (fun node ->
      spawn node (Slave.proc pid_to_send_to)
    ) >>= fun lst' ->
    let pids = List.map fst lst' in

    Format.printf "Line %s@;%!" __LOC__ ;

    let batches = H.XList.group_by batch_size requests in
    Format.printf "Line %s@;%!" __LOC__ ;
    let assigned_batches =
      batches |> List.mapi (fun i batch ->
        Format.printf "%d %d Line %s@;%!" (List.length pids) i __LOC__ ;
        let pid = List.nth pids (i mod nb_slaves) in
        Format.printf "%d Line %s@;%!" i __LOC__ ;
        (pid , batch)
      )
    in
    Format.printf "Line %s@;%!" __LOC__ ;
    assigned_batches |> XD.iteri_lst (fun i (pid , batch) ->
      Format.printf "Send signature request %d@;%!" i ;
      pid >! M.Signature_requests (i , batch)
    ) >>= fun () ->
    (* send messages to the spawned remote processes, using the infix funtion '>!' alias of send *)

    Format.printf "Line %s@;%!" __LOC__ ;


    pids |> XD.iter_lst (fun pid -> (
      let ids =
        assigned_batches
        |> List.mapi (fun i (pid' , _) -> (pid' , i))
        |> List.filter (fun (pid' , _) -> pid = pid')
        |> List.map (fun (_ , i) -> i)
      in
      pid >! M.Stop_slave_after ids
    )) >>= fun () ->

    let processes_terminated = ref 0 in         

    let check_failed = ref [] in
    (* process messages that are sent to us *)
    receive_loop (
      (case @@ function 
        | M.Signature_responses (id , failed) as v -> Some (fun () -> 
          Format.printf "got message %s from remote node@;%!"
          @@ M.string_of_message v ;
          let failed' = failed |> List.map (fun i -> id * batch_size + i) in
          check_failed := failed' @ !check_failed ;
          return true
        )
        | _ -> Some (fun () -> 
          fail Unexpected_msg
        )
      ) |. (termination_case @@ function
      | Normal _ -> 
        processes_terminated := !processes_terminated + 1 ;
        Format.printf "remote process terminated successfully, number of remote processes terminated %d@;%!"
          !processes_terminated ;                    
        return (!processes_terminated < nb_slaves)
      | _ -> assert false 
      )
    ) >>= fun () ->
    Format.printf "Loop Finished@;%!" ;
    assert (List.length !check_failed = 0) ;
    return ()
  
  let parallel_check' requests =
    Format.printf "Line %s@;%!" __LOC__ ;
    D.run_node ~process:(fun () -> parallel_check requests) config
end
end

let time f =
  let c = Mtime_clock.counter () in
  f () ;
  Mtime_clock.count c

let bench_time n =
  let l = Array.length data in
  let lst = List.init n @@ fun i -> (
    let i' = i mod l in
    let (pk , signature , bytes) = data.(i') in
    Signature_request.make_tpl pk signature bytes
  ) in
  let module M = Make() in
  let t = time @@ fun () -> (
    Format.printf "Line %s@;%!" __LOC__ ;
    Lwt_main.run @@ M.Master.parallel_check' lst
  ) in
  Format.printf "Bench %d: %a@;%!" n Mtime.Span.pp t ;
  ()



let () =
  (* data |> Array.iteri (fun i (pk , _signature , _bytes) -> (
    Format.printf "Key %d: 0x%a@;%!" i bytes_pp_hex @@
    Sodium.Sign.Bytes.of_public_key pk ;
  )) ; *)
  Printexc.record_backtrace true ;
  Format.printf "Line %s@;%!" __LOC__ ;
  (* bench_time 10 ; *)
  bench_time 100 ;
  (* bench_time 1_000 ;
  bench_time 10_000 ; *)
  ()
