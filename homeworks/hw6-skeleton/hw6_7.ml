exception TODO

type pgm = cmd
and cmd = ASSIGN of exp
 | SEQUENCE of cmd * cmd
 | REPEAT of cmd
 | CHOICE of cmd * cmd
 | EQ of exp * cmd
 | NEQ of exp * cmd
and exp = NUM of int
 | ADD of exp * exp
 | SUB of exp * exp
 | VAR

type state = int
type new_state = ST of state | ERROR of state



let comp x y =
	match (x,y) with
	| (ST a, ST b) | (ERROR a, ERROR b) -> compare a b
	| (ST a, ERROR b) -> -1
	| _ -> 1

let merge x y = List.sort_uniq comp (x @ y)

let rec eval e st =
	match e with
	| NUM i -> i
	| ADD (e1,e2) -> eval e1 st + eval e2 st
	| SUB (e1,e2) -> eval e1 st - eval e2 st
	| VAR -> st

let map p sts mapper = List.fold_left merge [] (List.map (fun k -> mapper p k) sts)

let rec unpack ns_l =
	match ns_l with
	| (ST x)::tl | (ERROR x)::tl -> x::unpack tl 
	| _ -> []

let rec exevall p sts =
	match p with
	| SEQUENCE (c1,c2) -> exevall c2 (exevall c1 sts)
	| REPEAT c -> if (merge sts (map c sts pass)) = sts then sts else exevall p (merge sts (map c sts pass))
	| CHOICE (c1,c2) -> merge (exevall c1 sts) (exevall c2 sts)
	| EQ _| NEQ _ | ASSIGN _ -> map p sts pass
and exev p st =
	match p with
	| ASSIGN e -> 	  
	  let v = eval e st in
	  if v < -5 || v > 5 then [ERROR st] else [ST v]
	| SEQUENCE (c1,c2) ->
	  map c2 (exev c1 st) pass
	| REPEAT c -> [ST st]
	| CHOICE (c1,c2) -> merge (exevall c1 [ST st]) (exevall c2 [ST st])
	| EQ (e,c) -> if (eval e st = st) then exevall c [ST st] else [ST st]
	| NEQ (e,c) -> if (eval e st != st) then exevall c [ST st] else [ST st]
and pass p x =
	match x with
	| ST st -> exev p st
	| _ -> [x]

let exeval p st =
	List.sort_uniq compare (unpack (exevall p [ST st]))