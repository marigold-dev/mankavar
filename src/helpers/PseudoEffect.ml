type 'a return = {
  return : 'b . 'a -> 'b ;
}

let returner (type r) f =
  let exception Return of r in
  let p = {
    return = fun x -> raise (Return x) ;
  } in
  try f p
  with Return r -> r

type 'a fail = {
  fail : 'b . 'a -> 'b ;
}

let fail_opt f =
  returner @@ fun { return } ->
  let fail () = return None in
  Option.some @@ f { fail }

let failer f =  
  returner @@ fun { return } ->
  let fail x = return @@ Result.error x in
  Result.ok @@ f { fail }
