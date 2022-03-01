open Das_helpers

module Value = struct
  type t = int64
  let succ = Int64.succ
  let gez x = Int64.compare x Int64.zero >= 0
  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul
  let div = Int64.div
  let true_ = Int64.one
  let false_ = Int64.zero
  let is_zero = Int64.equal Int64.zero
  let or_ a b = if is_zero a then b else a
  let and_ a b = if not @@ is_zero a then b else a
  let not_ a = if is_zero a then true_ else false_
  let zero = Int64.zero
  let of_int = Int64.of_int
  module Map = XMap.Make(Int64)
  let pp ppf = Format.fprintf ppf "%Ld"
end

module Stack = struct
  let stack_size = 10_000
  type t = {
    content : Value.t array ;
    mutable position : int ;
  }
  let empty = {
    content = Array.make stack_size Value.zero ;
    position = 0 ;
  }

  let push t v =
    t.position <- t.position + 1 ;
    t.content.(t.position) <- v
  [@@inline]

  let get t i = t.content.(t.position - i)
  [@@inline]

  let set t i v = t.content.(t.position - i) <- v
  [@@inline]

  let drop t i =
    t.position <- t.position - i
  [@@inline]

  let pp ppf s =
    let print x = Format.fprintf ppf x in
    let { content ; position } = s in
    print "@[" ;
    for i = 0 to position do
      print "%a" Value.pp content.(i) ;
      if i mod 10 = 0 then (
        print "(%d)" i ;
      ) ;
      print "@," ;
    done ;
    print "@]" ;
    ()
end