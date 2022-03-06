let () =
  let open Ml_main.TBytes.Raw in
  let ptr = malloc 50 in
  Format.printf "Raw ptr as int:\t%d@;%!" ptr ;
  Format.printf "Value int64:\t%Ld@;%!" @@ get_int64 ptr ;
  Format.printf "Value int:\t%d@;%!" @@ get_int ptr ;
  free ptr ;
  ()

