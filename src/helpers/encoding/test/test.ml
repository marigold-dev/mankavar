let test_idem a x =
  let open Encoding in
  assert (x = (of_bytes a @@ to_bytes a x))

type foobar =
| Foo of int
| Bar of string

let foobar_encoding =
  let open Encoding in
  union [
    case (function Foo i -> Some i | _ -> None) (fun i -> Foo i) int ;
    case (function Bar i -> Some i | _ -> None) (fun i -> Bar i) string ;
  ]

let () =
  Printexc.record_backtrace true ;
  let open Encoding in
  test_idem string "Lol" ;
  test_idem int 42 ;
  test_idem int32 64l ;
  test_idem int64 143L ;
  test_idem (list int) [23 ; 144] ;
  test_idem (pair int string) (42 , "wee") ;
  test_idem foobar_encoding (Foo 42) ;
  test_idem foobar_encoding (Bar "arghhhhh") ;
  ()