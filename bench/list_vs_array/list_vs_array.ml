let time f =
  let c = Mtime_clock.counter () in
  f () ;
  Mtime_clock.count c

let array_iter n =
  let data = Array.make n 0 in
  time @@ fun () ->
  let l = Array.length data in
  for i = 0 to l - 1 do
    data.(i) <- data.(i) + 1
  done ;
  ()

let list_iter n =
  let data = List.init n (fun _ -> ref 0) in
  time @@ fun () ->
  List.iter (fun r -> r := !r + 1) data ;
  ()
  
let bench n =
  let lst_time = list_iter n in
  let arr_time = array_iter n in
  let mpp = Mtime.Span.pp in
  Format.printf "Array\t(%d)\t:\t%a\n"
     n mpp arr_time ;
  Format.printf "List\t(%d)\t:\t%a\n"
    n mpp lst_time ;
  Format.printf "@;" ;
  ()

let () =
  bench 100_000 ;
  bench 1_000_000 ;
  bench 10_000_000 ;
  ()
