module Raw = struct
  type t =
  | Foo of int
  | Bar of (int * int)
  | Wee of string
  [@@deriving ez]
end

module type FBW = module type of Raw

module type FB = sig
  (* Generated with PPX *)
  type t
  type foo = int
  type bar = int * int
  val foo : foo -> t
  val bar : bar -> t
  val destruct : foo:(foo -> 'a) -> bar:(bar -> 'a) -> t -> 'a
end

module FBW : FBW = Raw
module FB : FB = struct
  type t = Raw.t
  type foo = Raw.foo
  type bar = Raw.bar
  let foo = Raw.foo
  let bar = Raw.bar
  let destruct ~foo ~bar t =
    Raw.destruct ~foo ~bar ~wee:(fun _ -> assert false) t
end

let pass : FBW.t -> FB.t =
  FBW.destruct
  ~foo:FB.foo
  ~bar:FB.bar
  ~wee:(fun x -> x |> int_of_string |> FB.foo)