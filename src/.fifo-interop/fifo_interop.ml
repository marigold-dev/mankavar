open Das_helpers

type 'a encoding = 'a Encoding.t

type handle = in_channel * out_channel


type runner =
| Command of string
| Fifo of string

let runner_open : runner -> _ = function
| Command x -> Unix.open_process_args x [||]
| Fifo x -> (
  let in_fd = Unix.openfile x [Unix.O_RDONLY] 0 in
  let out_fd = Unix.openfile x [Unix.O_WRONLY] 0 in
  (Unix.in_channel_of_descr in_fd ,
   Unix.out_channel_of_descr out_fd)
)

let runner_close : handle -> _ = fun (in_ , out) ->
  close_in in_ ;
  close_out out ;
  ()

module type PARAMETER = sig
  type message
  val message_encoding : message encoding
  type response
  val response_encoding : response encoding
  val runner : runner
end

module Make(P : PARAMETER)() = struct
  type message = P.message

  let lock = ref None
  let read_buffer_size = 65536
  let read_buffer = Bytes.make read_buffer_size '0'
  let write_buffer_size = 65536
  let write_buffer = Bytes.make write_buffer_size '0'
  let size_buffer = Bytes.make 8 '0'

  let open_ () =
    if Option.is_some !lock then assert false ;
    lock := Option.some @@ runner_open P.runner
  
  let close () =
    match !lock with
    | None -> assert false
    | Some x -> runner_close x ; lock := None ; ()

  let write message out =
    let message_bytes = Encoding.to_bytes P.message_encoding message in
    let message_length = Bytes.length message_bytes in
    Bytes.set_int64_le size_buffer 0 @@ Int64.of_int message_length ;
    output out size_buffer 0 8 ;
    let i = ref 0 in
    while !i < message_length do
      let l = min write_buffer_size (message_length - !i) in
      output out write_buffer !i l ;
      i := !i + l ;
    done ;
    ()
  
  

  let run
  : message -> bytes -> unit
  = fun message response_buffer ->
    let (in_ , out) = match !lock with
      | None -> assert false
      | Some x -> x
    in
    write message out ;
    let init_response_len = input in_ read_buffer 0 read_buffer_size in
    let response_size = Bytes.get_int64_be read_buffer 0 in
    if XInt64.(Int64.of_int @@ Bytes.length response_buffer <= response_size) then
      assert false ;

    let response_read =
      Fifoder.decode read_buffer read_buffer 0 response_to_read
    in
    Bytes.sub read_buffer 0 response_read

  let run_quick : message -> bytes
  = fun message ->
    open_ () ;
    let result = run message in
    close () ;
    result


end
