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

let merge x y = List.sort_uniq compare (x @ y)

let rec eval e st =
	match e with
	| NUM i -> i
	| ADD (e1,e2) -> eval e1 st + eval e2 st
	| SUB (e1,e2) -> eval e1 st - eval e2 st
	| VAR -> st


let rec exeval (p:pgm) (st:state): state list =
	match p with
	| ASSIGN e -> 
	  let v = eval e st in
	  if v < -5 || v > 5 then [] else [v]
	| SEQUENCE (c1,c2) -> 
	  List.fold_left merge [] (List.map (fun x -> exeval c2 x) (exeval c1 st))
	| REPEAT c -> 
	  if ((exeval c st) = (exeval (SEQUENCE(c,c)) st)) then merge [st] (exeval c st) else merge [st] (exeval (SEQUENCE(c,(REPEAT c))) st)
	| CHOICE (c1,c2) -> merge (exeval c1 st) (exeval c2 st)
	| EQ (e,c) -> if (eval e st = st) then exeval c st else [st]
	| NEQ (e,c) -> if (eval e st != st) then exeval c st else [st]

