let mpp = Mtime.Span.pp
let time f =
  let c = Mtime_clock.counter () in
  let ret = f () in
  ret , Mtime_clock.count c

  
let bench () =
  let is_parent = ref true in
  let (pin , pout) = Unix.pipe () in
  let out = Bytes.make 1_000_000 '0' in
  let () , fork_time = time @@ fun () -> is_parent := Unix.fork () <> 0 in
  Format.printf "Fork time: %a\n" mpp fork_time ;
  if !is_parent then (
    let in_ , alloc_time = time @@ fun () -> Bytes.make 1_000_000 '0' in
    Format.printf "alloc time: %a\n%!" mpp alloc_time ;
    let () , read_time =
      time @@ fun () ->
      let total_read = ref 0 in
      while !total_read < 1_000_000_000 do
        let read = Unix.read pin in_ 0 1_000_000 in
        (* Format.printf "read:%d\n%!" read ; *)
        total_read := !total_read + read ;
        (* assert (read = 1_000_000) ; *)
        ()
      done ;
    in
    Format.printf "Read time: %a\n" mpp read_time
  ) else (
    for _i = 1 to 1_000 do
      let written = Unix.write pout out 0 1_000_000 in
      Format.printf "written:%d\n" written ;
      ()
    done ;
  ) ;
  Format.printf "@;" ;
  if not (!is_parent) then Unix._exit 0 ;
  ()

let () =
  bench () ;
  ()
