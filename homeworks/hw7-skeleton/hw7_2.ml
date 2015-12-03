module type SKI = sig
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)
  val react: liquid -> liquid
  val pprint: liquid -> string
end

module SkiLiquid : SKI = struct
  exception ETODO

  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)

  let rec react: liquid -> liquid =
    fun l ->
    if l = process l then l else react (process l)


  and process l =
    match l with
    | M (I, x) -> x
    | M (M (K, x), _) -> x
    | M (M (M (S, x), y), z) -> (M (M (x, z), M (y, z)))
    | M (x,y) -> (M (react x, react y))
    | _ -> l

  let rec pprint: liquid -> string =
    fun l ->
    match l with
    | S -> "S"
    | K -> "K"
    | I -> "I"
    | M (x,y) -> "("^(pprint x)^" "^(pprint y)^")"
    | V s -> s
end
