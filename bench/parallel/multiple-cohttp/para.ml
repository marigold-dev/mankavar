module H = Das_helpers

let time f =
  let c = Mtime_clock.counter () in
  let ret = f () in
  ret , Mtime_clock.count c

let bytes_pp_hex ppf b =
  b |> Bytes.iter @@ fun c ->
    let i = Char.code c in
    Format.fprintf ppf "%x" i ;
    ()

let public_key_pp ppf x =
  Format.fprintf ppf "%a" bytes_pp_hex @@
  Sodium.Sign.Bytes.of_public_key x

let public_key_encoding =
  let open H.Encoding in
  conv Sodium.Sign.Bytes.to_public_key Sodium.Sign.Bytes.of_public_key @@
  bytes

let port = 7500

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
  
  let encoding =
    let open H.Encoding in
    conv make_tpl' destruct @@
    tuple_3 public_key_encoding bytes bytes

  let process t =
    let pk , sign , bytes = destruct t in
    let bytes' = Sodium.Sign.Bytes.sign_open pk sign in
    bytes = bytes'
end

module Signature_responses = struct
  type t = int * int list
  let encoding =
    let open H.Encoding in
    pair int (list int)
end

module Signature_requests = struct
  type t = int * Signature_request.t list
  let encoding : t H.Encoding.t =
    let open H.Encoding in
    pair int (list Signature_request.encoding)

  let process ((id , reqs) : t) : Signature_responses.t =
    let responses =
      reqs
      |> List.mapi (fun i x -> i , Signature_request.process x)
      |> List.filter_map (fun (i , b) -> if not b then Some i else None)
    in
    id , responses
end

module Para_request = struct
  type t =
  | Perform of Signature_requests.t
  | Die of int
  [@@deriving ez]

  let encoding =
    let open Encoding in
    union [
      case get_perform_opt perform Signature_requests.encoding ;
      case get_die_opt die int ;
    ]
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

let print i x = ignore @@ Unix.system @@
  Format.asprintf "echo \"%s\" >> ~/Programming/das/_build/toto%d" x i


module Slave = struct
  open Cohttp
  open Cohttp_lwt_unix
  open Lwt

  let respond body =
    Server.respond_string ~status:`OK ~body ()

  let main i =
    let (stop , stopper) = Lwt.wait () in
    let count = ref 0 in
    let max_count = ref (-1) in
    let try_die () =
      count := !count + 1 ;
      if !max_count <> -1 && !max_count = !count then
      Lwt.wakeup_later stopper ()
    in
    let callback _conn req body =
      (* print i "receiving request" ; *)
      (* Format.printf "receiving requests@;%!" ; *)
      body |> Cohttp_lwt.Body.to_string >>= fun body_str ->
      let body_bytes = Bytes.of_string body_str in
      let para_request = H.Encoding.of_bytes
        Para_request.encoding body_bytes in
      match para_request with
      | Perform sig_requests -> (
        (* Format.printf "receiving sig requests@;%!" ; *)
        let sig_responses = Signature_requests.process sig_requests in
        let bytes =
          sig_responses
          |> H.Encoding.to_bytes Signature_responses.encoding
        in
        (* Format.printf "Responding with response of size %d@;%!"
          @@ Bytes.length bytes ; *)
        try_die () ;
        bytes
        |> Bytes.to_string
        |> respond
      )
      | Die n -> (
        (* print i "receive die request " ; *)
        (* Format.printf "DIE  %d@;%!" n ; *)
        max_count := n ;
        try_die () ;
        respond ""
      )
    in
    (* print i @@ Format.asprintf "Port: %d" @@ port + i ; *)
    let server = Server.create ~stop ~mode:(`TCP (`Port (port + i)))
      (Server.make ~callback ()) in
    server >>= fun server ->
    return server

end

let nb_slaves = 8
let batch_size = 1000


module Master = struct

  open Lwt
  open Cohttp
  open Cohttp_lwt_unix
  open Conduit_lwt_unix
  
  let mk_uri i =
    Uri.of_string @@
    Format.asprintf "http://127.0.0.1:%d" @@ port + i

  let parallel_check requests =
    let batches = H.XList.group_by batch_size requests in
    let assigned_batches =
      batches |> List.mapi (fun i batch ->
        let nid = i mod nb_slaves in
        (nid , batch)
      )
    in
    let responses_lwt =
      assigned_batches |> Lwt_list.mapi_p (fun i (nid , batch) -> (
        let body , t =
          time @@ fun () ->
          let bytes = H.Encoding.to_bytes
            Para_request.encoding @@ Para_request.perform (i , batch) in
          let str = Bytes.to_string bytes in
          Cohttp_lwt.Body.of_string str
        in
        (* Format.printf "encoding time: %a@;%!" Mtime.Span.pp t ; *)
        Client.post ~body (mk_uri nid) >>= fun (resp, body) ->
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        body
      ))
    in
    responses_lwt >>= fun responses_str ->
    let responses =
      responses_str
      |> List.map Bytes.of_string
      |> List.map (H.Encoding.of_bytes Signature_responses.encoding)
    in
    (* print 42 "responses" ; *)
    responses |> List.iter (fun (_ , lst) -> 
      assert (List.length lst = 0) ;
      ()
    ) ;
    return ()
end



let bench_time n =
  let l = Array.length data in
  let lst = List.init n @@ fun i -> (
    let i' = i mod l in
    let (pk , signature , bytes) = data.(i') in
    Signature_request.make_tpl pk signature bytes
  ) in
  (* forks nb_slaves Slave.main @@ fun () -> *)
  (* print 100 "start parent" ; *)
  let () , t = time @@ fun () -> (
    Lwt_main.run @@ Master.parallel_check lst
  ) in
  Format.printf "Bench  %d: %a@;%!" n Mtime.Span.pp t ;
  ()

let () =
  if (Array.length Sys.argv > 1) && Sys.argv.(1) = "slave" then (
    Lwt_main.run @@ Slave.main @@ int_of_string Sys.argv.(2)
  ) else (
    Printexc.record_backtrace true ;
    (* bench_time 10 ; *)
    Format.printf "Line %s@;%!\n" __LOC__ ;
    (* bench_time 50 ; *)
    (* bench_time 1_000 ; *)
    bench_time 10_000 ;
    bench_time 20_000 ;
    bench_time 50_000 ;
  ) ;
  ()
