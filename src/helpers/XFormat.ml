type 'a pp = Format.formatter -> 'a -> unit

let array f ppf xs =
  Format.fprintf ppf "@[<hov 2>{ " ;
  xs |> Array.iter (fun x ->
    Format.fprintf ppf "%a" f x
  ) ;
  Format.fprintf ppf "}@]" ;
  ()

let list f ppf xs =
  Format.fprintf ppf "@[<hov 2>[ " ;
  xs |> List.iter (fun x ->
    Format.fprintf ppf "%a" f x
  ) ;
  Format.fprintf ppf "]@]" ;
  ()

let tuple_2 a b ppf (x , y) =
  Format.fprintf ppf "@[<hov 2>( %a , @; %a )@]" a x b y
let pair = tuple_2

let conv f pp ppf x = pp ppf @@ f x
let unit ppf () = Format.fprintf ppf "()"
let int64 ppf = Format.fprintf ppf "%Ld"
let int64' = Fun.flip int64
let int ppf = Format.fprintf ppf "%d"
let prefix x f ppf = Format.fprintf ppf "%s%a" x f
let cst x ppf _ = Format.fprintf ppf "%s" x
let cst' x y z = cst x z y